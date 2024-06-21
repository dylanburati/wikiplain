// Based on the Python code in this blog post and associated repo:
// https://explosion.ai/blog/part-of-speech-pos-tagger-in-python
// https://github.com/sloria/textblob-aptagger/tree/master

use std::cmp::Ordering;
use std::collections::hash_map::Entry::*;

use anyhow::Result;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;

// const PENN_TAG_NUMSIGN: usize = 0;
// const PENN_TAG_DOLLAR: usize = 1;
// const PENN_TAG_COMMA: usize = 2;
// const PENN_TAG_PERIOD: usize = 3;
// const PENN_TAG_CC: usize = 4;
// const PENN_TAG_CD: usize = 5;
// const PENN_TAG_DT: usize = 6;
// const PENN_TAG_EX: usize = 7;
// const PENN_TAG_FW: usize = 8;
// const PENN_TAG_IN: usize = 9;
// const PENN_TAG_JJ: usize = 10;
// const PENN_TAG_JJR: usize = 11;
// const PENN_TAG_JJS: usize = 12;
// const PENN_TAG_LS: usize = 13;
// const PENN_TAG_MD: usize = 14;
// const PENN_TAG_NN: usize = 15;
pub const PENN_TAG_NNP: usize = 16;
pub const PENN_TAG_NNPS: usize = 17;
// const PENN_TAG_NNS: usize = 18;
// const PENN_TAG_PDT: usize = 19;
// const PENN_TAG_POS: usize = 20;
// const PENN_TAG_PRP: usize = 21;
// const PENN_TAG_PRPZ: usize = 22;
// const PENN_TAG_RB: usize = 23;
// const PENN_TAG_RBR: usize = 24;
// const PENN_TAG_RBS: usize = 25;
// const PENN_TAG_RP: usize = 26;
// const PENN_TAG_SYM: usize = 27;
// const PENN_TAG_TO: usize = 28;
// const PENN_TAG_UH: usize = 29;
// const PENN_TAG_VB: usize = 30;
// const PENN_TAG_VBD: usize = 31;
// const PENN_TAG_VBG: usize = 32;
// const PENN_TAG_VBN: usize = 33;
// const PENN_TAG_VBP: usize = 34;
// const PENN_TAG_VBZ: usize = 35;
// const PENN_TAG_WDT: usize = 36;
// const PENN_TAG_WP: usize = 37;
// const PENN_TAG_WPZ: usize = 38;
// const PENN_TAG_WRB: usize = 39;
pub const PENN_TAG_BEGIN: usize = 40;
pub const C: usize = 40;

#[derive(Serialize, Deserialize)]
struct FeatClassRecord {
    class_id: usize,
    // The weight assigned in the latest update of the model
    weight: f32,
    // The sum of (model updates at weight * weight) over the lifetime of the record,
    // up to the iteration in `timestamp`
    total: f32,
    // The last iteration at which the weight was modified
    timestamp: u64,
}

#[derive(Serialize, Deserialize)]
struct FeatRecord {
    classes: SmallVec<[FeatClassRecord; 2]>,
    observations: u64,
}

impl FeatRecord {
    fn new() -> FeatRecord {
        FeatRecord {
            classes: SmallVec::default(),
            observations: 1,
        }
    }

    fn append_feat_class_record(&mut self, record: FeatClassRecord) {
        self.classes.push(record);
    }

    fn get_feat_class_record(&mut self, class_id: usize) -> Option<&mut FeatClassRecord> {
        self.classes.iter_mut().find(|c| c.class_id == class_id)
    }
}

#[derive(Serialize)]
pub struct Prediction {
    pub scores: Vec<f32>,
    pub observations: u64,
}

#[derive(Serialize, Deserialize)]
pub struct AveragedPerceptron {
    data: FxHashMap<String, FeatRecord>,
    iteration: u64,
}

impl AveragedPerceptron {
    fn new() -> AveragedPerceptron {
        AveragedPerceptron {
            data: FxHashMap::default(),
            iteration: 0,
        }
    }

    fn predict(&self, primary_feature: String, features: &[String]) -> Prediction {
        let mut scores = [0f32; C];
        let mut observations = 0;
        if let Some(record) = self.data.get(&primary_feature) {
            for c in record.classes.iter() {
                scores[c.class_id] += c.weight;
            }
            observations = record.observations;
        }
        for key in features {
            if let Some(record) = self.data.get(key) {
                for c in record.classes.iter() {
                    scores[c.class_id] += c.weight;
                }
            }
        }
        Prediction {
            scores: Vec::from(scores),
            observations,
        }
    }

    fn predict_and_update(&mut self, features: &[String], truth: usize) -> usize {
        let mut scores = [0f32; C];
        for key in features {
            match self.data.entry(key.to_owned()) {
                Occupied(mut occupied) => {
                    let record = occupied.get_mut();
                    record.observations += 1;
                    for c in record.classes.iter() {
                        scores[c.class_id] += c.weight;
                    }
                }
                Vacant(vacant) => {
                    vacant.insert(FeatRecord::new());
                }
            }
        }
        let (guess, _) = scores
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
            .unwrap();
        if truth != guess {
            for key in features {
                let record = self.data.get_mut(key).unwrap();
                if let Some(r1) = record.get_feat_class_record(truth) {
                    r1.total += ((self.iteration - r1.timestamp) as f32) * r1.weight;
                    r1.timestamp = self.iteration;
                    r1.weight += 1.0;
                } else {
                    record.append_feat_class_record(FeatClassRecord {
                        class_id: truth,
                        weight: 1.0,
                        total: 0.0,
                        timestamp: self.iteration,
                    })
                }
                if let Some(r2) = record.get_feat_class_record(guess) {
                    r2.total += ((self.iteration - r2.timestamp) as f32) * r2.weight;
                    r2.timestamp = self.iteration;
                    r2.weight -= 1.0;
                } else {
                    record.append_feat_class_record(FeatClassRecord {
                        class_id: guess,
                        weight: -1.0,
                        total: 0.0,
                        timestamp: self.iteration,
                    })
                }
            }
        }
        self.iteration += 1;
        guess
    }

    fn average_weights(&mut self) {
        for record in self.data.values_mut() {
            for c in record.classes.iter_mut() {
                if c.weight != 0f32 {
                    c.total += ((self.iteration - c.timestamp) as f32) * c.weight;
                    c.timestamp = self.iteration;
                }
                c.weight = c.total / (self.iteration as f32);
            }
        }
    }
}

pub fn train<I>(model: &mut AveragedPerceptron, input: &mut I) -> Result<f32>
where
    I: Iterator<Item = serde_json::Result<Vec<(String, usize)>>>,
{
    let mut correct = 0u64;
    let mut total = 0u64;
    for (sentence_num, sentence_res) in input.enumerate() {
        let sentence: Vec<(String, usize)> = sentence_res?;
        let mut context = vec!["_START2".to_owned(), "_START1".to_owned()];
        context.extend(sentence.iter().cloned().map(|e| e.0));
        context.push(".".to_owned());
        context.push("_END".to_owned());
        let mut prev_guess = PENN_TAG_BEGIN;
        for (i, (word, truth)) in sentence.into_iter().enumerate() {
            prev_guess = model.predict_and_update(
                &[
                    "bias".to_string(),
                    (word + "_0"),
                    (context[i].clone() + "_-2"),
                    (context[i + 1].clone() + "_-1"),
                    (context[i + 3].clone() + "_+1"),
                    (prev_guess.to_string() + "_t"),
                    (prev_guess.to_string() + "_" + context[i + 1].as_str() + "_t-1"),
                ],
                truth,
            );
            if prev_guess == truth {
                correct += 1;
            }
            total += 1;
        }
        if (sentence_num + 1) % 1000 == 0 {
            eprint!("\r\x1b[K{}", sentence_num + 1);
        }
    }
    eprintln!();
    eprintln!("Trained; averaging weights...");
    model.data.retain(|_, rec| rec.observations >= 3);
    model.data.shrink_to_fit();
    model.average_weights();
    Ok((correct as f32) / (total as f32))
}

pub fn predict(model: &AveragedPerceptron, sentence: &[String]) -> Vec<Prediction> {
    let mut context = vec!["_START2", "_START1"];
    context.extend(sentence.iter().map(|s| s.as_str()));
    context.push(".");
    context.push("_END");
    let mut prev_guess = PENN_TAG_BEGIN;
    let mut predictions = vec![];
    for (i, word) in sentence.into_iter().enumerate() {
        let prediction = model.predict(
            word.to_string() + "_0",
            &[
                "bias".to_string(),
                (context[i].to_string() + "_-2"),
                (context[i + 1].to_string() + "_-1"),
                (context[i + 3].to_string() + "_+1"),
                (prev_guess.to_string() + "_t"),
                (prev_guess.to_string() + "_" + context[i + 1] + "_t-1"),
            ],
        );
        (prev_guess, _) = prediction
            .scores
            .iter()
            .enumerate()
            .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
            .unwrap();
        predictions.push(prediction);
    }
    predictions
}
