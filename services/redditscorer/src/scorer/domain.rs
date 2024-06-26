/// Converted from https://gist.github.com/danallison/905746161b23832b9cd8029ca6919b51
/// https://en.wikipedia.org/wiki/Second-level_domain
/// https://www.quackit.com/domain-names/country_domain_extensions.cfm
pub(crate) fn is_second_level_domain(s: &str) -> bool {
    matches!(
        s,
        // Afghanistan
        "net.af" |
        "org.af" |
        "gov.af" |
        "edu.af" |
        "com.af" |

        // Albania
        "net.al" |
        "org.al" |
        "gov.al" |
        "edu.al" |
        "com.al" |

        // Algeria
        "art.dz" |
        "net.dz" |
        "asso.dz" |
        "org.dz" |
        "gov.dz" |
        "pol.dz" |
        "edu.dz" |
        "com.dz" |

        // Andorra
        "m.ad" |
        "n.ad" |
        "o.ad" |

        // Angola
        "gv.ao" |
        "co.ao" |
        "it.ao" |
        "ed.ao" |
        "og.ao" |
        "pb.ao" |

        // Anguilla
        "com.ai" |
        "off.ai" |
        "net.ai" |
        "org.ai" |

        // Antigua and Barbuda
        "net.ag" |
        "co.ag" |
        "nom.ag" |
        "org.ag" |
        "edu.ag" |
        "com.ag" |

        // Argentina
        "mil.ar" |
        "net.ar" |
        "musica.ar" |
        "tur.ar" |
        "org.ar" |
        "gov.ar" |
        "edu.ar" |
        "com.ar" |
        "gob.ar" |
        "int.ar" |

        // Aruba
        "com.aw" |

        // Ascension Island
        "net.ac" |
        "mil.ac" |
        "org.ac" |
        "gov.ac" |
        "com.ac" |

        // Australia
        "otc.au" |
        "id.au" |
        "wa.au" |
        "gw.au" |
        "oz.au" |
        "info.au" |
        "qld.au" |
        "conf.au" |
        "org.au" |
        "act.au" |
        "asn.au" |
        "edu.au" |
        "nsw.au" |
        "com.au" |
        "nt.au" |
        "vic.au" |
        "gov.au" |
        "tas.au" |
        "net.au" |
        "csiro.au" |
        "archie.au" |
        "telememo.au" |
        "sa.au" |

        // Austria
        "priv.at" |
        "gv.at" |
        "co.at" |
        "or.at" |
        "ac.at" |

        // Azerbaijan
        "biz.az" |
        "net.az" |
        "mil.az" |
        "pro.az" |
        "name.az" |
        "org.az" |
        "gov.az" |
        "pp.az" |
        "edu.az" |
        "info.az" |
        "com.az" |
        "int.az" |

        // Bahamas
        "net.bs" |
        "org.bs" |
        "gov.bs" |
        "edu.bs" |
        "com.bs" |

        // Bahrain
        "biz.bh" |
        "cc.bh" |
        "net.bh" |
        "org.bh" |
        "gov.bh" |
        "edu.bh" |
        "info.bh" |
        "com.bh" |

        // Bangladesh
        "net.bd" |
        "mil.bd" |
        "org.bd" |
        "gov.bd" |
        "edu.bd" |
        "com.bd" |
        "ac.bd" |

        // Barbados
        "biz.bb" |
        "net.bb" |
        "co.bb" |
        "org.bb" |
        "tv.bb" |
        "gov.bb" |
        "info.bb" |
        "store.bb" |
        "com.bb" |

        // Belize
        "net.bz" |
        "org.bz" |
        "gov.bz" |
        "edu.bz" |
        "com.bz" |

        // Benin
        "gouv.bj" |
        "edu .bj" |
        "mil.bj" |
        "asso.bj" |
        "barreau.bj" |
        "gov .bj" |
        "com .bj" |

        // Bolivia
        "mil.bo" |
        "net.bo" |
        "org.bo" |
        "tv.bo" |
        "gov.bo" |
        "edu.bo" |
        "com.bo" |
        "gob.bo" |
        "int.bo" |

        // Bosnia and Herzegovina
        "unbi.ba" |
        "rs.ba" |
        "net.ba" |
        "mil.ba" |
        "co.ba" |
        "org.ba" |
        "unmo.ba" |
        "untz.ba" |
        "gov.ba" |
        "edu.ba" |
        "com.ba" |
        "unze.ba" |
        "unsa.ba" |

        // Botswana
        "org.bw" |
        "co.bw" |

        // Brazil
        "imb.br" |
        "cng.br" |
        "mus.br" |
        "etc.br" |
        "rec.br" |
        "teo.br" |
        "tur.br" |
        "vlog.br" |
        "inf.br" |
        "slg.br" |
        "cim.br" |
        "fm.br" |
        "agr.br" |
        "eng.br" |
        "adv.br" |
        "am.br" |
        "ato.br" |
        "mat.br" |
        "vet.br" |
        "ecn.br" |
        "ppg.br" |
        "lel.br" |
        "flog.br" |
        "org.br" |
        "tv.br" |
        "fot.br" |
        "edu.br" |
        "eti.br" |
        "fnd.br" |
        "psc.br" |
        "adm.br" |
        "coop.br" |
        "com.br" |
        "bmd.br" |
        "radio.br" |
        "srv.br" |
        "tmp.br" |
        "far.br" |
        "arq.br" |
        "esp.br" |
        "ind.br" |
        "mil.br" |
        "pro.br" |
        "taxi.br" |
        "blog.br" |
        "nom.br" |
        "fst.br" |
        "zlg.br" |
        "gov.br" |
        "psi.br" |
        "not.br" |
        "g12.br" |
        "art.br" |
        "b.br" |
        "bio.br" |
        "net.br" |
        "wiki.br" |
        "jor.br" |
        "ntr.br" |
        "ggf.br" |
        "jus.br" |
        "odo.br" |
        "trd.br" |
        "med.br" |
        "qsl.br" |
        "cnt.br" |
        "leg.br" |

        // Brunei Darussalam
        "com.bn" |
        "edu.bn" |
        "org.bn" |
        "net.bn" |

        // Burma (Union of Myanmar)
        "net.mm" |
        "org.mm" |
        "gov.mm" |
        "edu.mm" |
        "com.mm" |

        // Cambodia
        "per.kh" |
        "mil.kh" |
        "net.kh" |
        "org.kh" |
        "gov.kh" |
        "edu.kh" |
        "com.kh" |

        // Cameroon
        "gov.cm" |

        // Canada
        "nb.ca" |
        "on.ca" |
        "ns.ca" |
        "qc.ca" |
        "sk.ca" |
        "ab.ca" |
        "nt.ca" |
        "nf.ca" |
        "yk.ca" |
        "bc.ca" |
        "nu.ca" |
        "pe.ca" |
        "mb.ca" |
        "nl.ca" |

        // Cayman Islands
        "net.ky" |
        "org.ky" |
        "gov.ky" |
        "edu.ky" |
        "com.ky" |

        // Chile
        "gov.cl" |
        "gob.cl" |

        // China
        "tw.cn" |
        "hi.cn" |
        "gd.cn" |
        "gs.cn" |
        "hn.cn" |
        "gx.cn" |
        "sh.cn" |
        "tj.cn" |
        "org.cn" |
        "qh.cn" |
        "xz.cn" |
        "edu.cn" |
        "ha.cn" |
        "jl.cn" |
        "zj.cn" |
        "com.cn" |
        "cq.cn" |
        "ah.cn" |
        "sd.cn" |
        "hb.cn" |
        "sc.cn" |
        "sn.cn" |
        "gov.cn" |
        "hl.cn" |
        "fj.cn" |
        "yn.cn" |
        "js.cn" |
        "nx.cn" |
        "bj.cn" |
        "net.cn" |
        "nm.cn" |
        "ln.cn" |
        "sx.cn" |
        "jx.cn" |
        "xj.cn" |
        "ac.cn" |
        "gz.cn" |
        "he.cn" |

        // Colombia
        "net.co" |
        "mil.co" |
        "nom.co" |
        "org.co" |
        "gov.co" |
        "edu.co" |
        "com.co" |

        // Comoros
        "tm.km" |
        "medecin.km" |
        "veterinaire.km" |
        "presse.km" |
        "gouv.km" |
        "mil.km" |
        "asso.km" |
        "nom.km" |
        "coop.km" |
        "edu.km" |
        "com.km" |
        "notaires.km" |
        "pharmaciens.km" |

        // Cook Islands
        "gen.ck" |
        "biz.ck" |
        "net.ck" |
        "co.ck" |
        "org.ck" |
        "gov.ck" |
        "edu.ck" |
        "info.ck" |

        // Costa Rica
        "go.cr" |
        "sa.cr" |
        "fi.cr" |
        "co.cr" |
        "or.cr" |
        "ed.cr" |
        "ac.cr" |

        // Cyprus | Republic of
        "tm.cy" |
        "biz.cy" |
        "pro.cy" |
        "net.cy" |
        "press.cy" |
        "name.cy" |
        "org.cy" |
        "gov.cy" |
        "ltd.cy" |
        "com.cy" |
        "ekloges.cy" |
        "ac.cy" |
        "parliament.cy" |

        // Dominica
        "com.dm" |
        "net.dm" |
        "org.dm" |

        // Dominican Republic
        "sld.do" |
        "art.do" |
        "net.do" |
        "mil.do" |
        "org.do" |
        "gov.do" |
        "edu.do" |
        "com.do" |
        "web.do" |
        "gob.do" |

        // East Timor
        "gov.tl" |
        "com.tl" |

        // Ecuador
        "net.ec" |
        "pro.ec" |
        "mil.ec" |
        "fin.ec" |
        "org.ec" |
        "gov.ec" |
        "edu.ec" |
        "info.ec" |
        "med.ec" |
        "com.ec" |

        // Eygypt
        "mil.eg" |
        "net.eg" |
        "name.eg" |
        "eun.eg" |
        "org.eg" |
        "gov.eg" |
        "edu.eg" |
        "sci.eg" |
        "com.eg" |

        // El Salvador
        "org.sv" |
        "red.sv" |
        "edu.sv" |
        "com.sv" |
        "gob.sv" |

        // Eritrea
        "mil.er" |
        "net.er" |
        "ind.er" |
        "rochest.er" |
        "org.er" |
        "gov.er" |
        "edu.er" |
        "com.er" |
        "w.er" |

        // Ethiopia
        "biz.et" |
        "net.et" |
        "name.et" |
        "org.et" |
        "gov.et" |
        "edu.et" |
        "info.et" |
        "com.et" |

        // Falkland Islands
        "net.fk" |
        "co.fk" |
        "nom.fk" |
        "org.fk" |
        "gov.fk" |
        "ac.fk" |

        // Fiji
        "biz.fj" |
        "mil.fj" |
        "net.fj" |
        "pro.fj" |
        "name.fj" |
        "org.fj" |
        "info.fj" |
        "com.fj" |
        "ac.fj" |

        // France
        "tm.fr" |
        "gouv.fr" |
        "presse.fr" |
        "asso.fr" |
        "nom.fr" |
        "com.fr" |
        "prd.fr" |

        // French Polynesia
        "com.pf" |

        // Ghana
        "mil.gh" |
        "org.gh" |
        "gov.gh" |
        "edu.gh" |
        "com.gh" |

        // Greece
        "net.gr" |
        "org.gr" |
        "gov.gr" |
        "edu.gr" |
        "com.gr" |

        // Guadeloupe
        "net.gp" |
        "asso.gp" |
        "org.gp" |
        "edu.gp" |
        "com.gp" |
        "mobi.gp" |

        // Guam
        "com.gu" |

        // Guatemala
        "net.gt" |
        "mil.gt" |
        "ind.gt" |
        "org.gt" |
        "edu.gt" |
        "com.gt" |
        "gob.gt" |

        // Guernsey
        "net.gg" |
        "co.gg" |
        "org.gg" |
        "gov.gg" |
        "sch.gg" |
        "ac.gg" |

        // Guinea
        "net.gn" |
        "org.gn" |
        "gov.gn" |
        "com.gn" |
        "ac.gn" |

        // Guyana
        "com.gy" |
        "net.gy" |
        "co.gy" |

        // Hong Kong
        "net.hk" |
        "idv.hk" |
        "org.hk" |
        "gov.hk" |
        "edu.hk" |
        "com.hk" |

        // Hungary
        "tm.hu" |
        "szex.hu" |
        "media.hu" |
        "konyvelo.hu" |
        "casino.hu" |
        "tozsde.hu" |
        "utazas.hu" |
        "2000.hu" |
        "info.hu" |
        "suli.hu" |
        "erotica.hu" |
        "hotel.hu" |
        "priv.hu" |
        "co.hu" |
        "jogasz.hu" |
        "forum.hu" |
        "erotika.hu" |
        "org.hu" |
        "news.hu" |
        "sport.hu" |
        "games.hu" |
        "film.hu" |
        "video.hu" |
        "agrar.hu" |
        "ingatlan.hu" |
        "city.hu" |
        "shop.hu" |
        "reklam.hu" |
        "sex.hu" |
        "lakas.hu" |
        "bolt.hu" |

        // India
        "gen.in" |
        "net.in" |
        "firm.in" |
        "co.in" |
        "ind.in" |
        "mil.in" |
        "org.in" |
        "gov.in" |
        "nic.in" |
        "edu.in" |
        "ac.in" |
        "res.in" |

        // Indonesia
        "go.id" |
        "net.id" |
        "co.id" |
        "mil.id" |
        "or.id" |
        "web.id" |
        "sch.id" |
        "ac.id" |

        // Iran
        "net.ir" |
        "id.ir" |
        "co.ir" |
        "org.ir" |
        "gov.ir" |
        "sch.ir" |
        "ac.ir" |

        // Iraq
        "mil.iq" |
        "org.iq" |
        "gov.iq" |
        "edu.iq" |
        "com.iq" |

        // Israel
        "net.il" |
        "co.il" |
        "muni.il" |
        "k12.il" |
        "org.il" |
        "gov.il" |
        "idf.il" |
        "ac.il" |

        // Isle of Man
        // "net.co" |
        "co.co" |
        // "org.co" |
        // "gov.co" |
        "ltd.co" |
        // "com.co" |
        "ac.co" |
        "plc.co" |

        // Jamaica
        "net.jm" |
        "mil.jm" |
        "org.jm" |
        "gov.jm" |
        "edu.jm" |
        "com.jm" |

        // Japan
        "go.jp" |
        "co.jp" |
        "ne.jp" |
        "ad.jp" |
        "gr.jp" |
        "lg.jp" |
        "or.jp" |
        "ed.jp" |
        "ac.jp" |

        // Jersey
        "net.je" |
        "co.je" |
        "org.je" |
        "gov.je" |
        "sch.je" |

        // Jordan
        "net.jo" |
        "mil.jo" |
        "name.jo" |
        "org.jo" |
        "gov.jo" |
        "edu.jo" |
        "com.jo" |
        "sch.jo" |

        // Kazakhstan
        "net.kz" |
        "mil.kz" |
        "org.kz" |
        "gov.kz" |
        "edu.kz" |
        "com.kz" |

        // Kenya
        "go.ke" |
        "co.ke" |
        "ne.ke" |
        "sc.ke" |
        "or.ke" |
        "ac.ke" |

        // Kiribati
        "phone.ki" |
        "biz.ki" |
        "net.ki" |
        "tel.ki" |
        "org.ki" |
        "mob.ki" |
        "gov.ki" |
        "edu.ki" |
        "info.ki" |
        "com.ki" |

        // Kuwait
        "net.kw" |
        "org.kw" |
        "gov.kw" |
        "edu.kw" |
        "com.kw" |

        // Kyrgyzstan
        "gov.kg" |
        "mil.kg" |

        // Latvia
        "mil.lv" |
        "id.lv" |
        "net.lv" |
        "org.lv" |
        "gov.lv" |
        "asn.lv" |
        "edu.lv" |
        "com.lv" |
        "conf.lv" |

        // Lebanon
        "net.lb" |
        "org.lb" |
        "gov.lb" |
        "edu.lb" |
        "com.lb" |

        // Lesotho
        "org.ls" |
        "co.ls" |

        // Liberia
        "net.lr" |
        "org.lr" |
        "gov.lr" |
        "edu.lr" |
        "com.lr" |

        // Libya
        "net.ly" |
        "id.ly" |
        "org.ly" |
        "gov.ly" |
        "edu.ly" |
        "med.ly" |
        "com.ly" |
        "sch.ly" |
        "plc.ly" |

        // Macau
        "net.mo" |
        "org.mo" |
        "gov.mo" |
        "edu.mo" |
        "com.mo" |

        // Macedonia (Republic of Macedonia)
        "net.mk" |
        "name.mk" |
        "org.mk" |
        "inf.mk" |
        "gov.mk" |
        "edu.mk" |
        "com.mk" |

        // Madagascar
        "tm.mg" |
        "mil.mg" |
        "nom.mg" |
        "org.mg" |
        "gov.mg" |
        "edu.mg" |
        "com.mg" |
        "prd.mg" |

        // Malawi
        "museum.mw" |
        "net.mw" |
        "co.mw" |
        "org.mw" |
        "gov.mw" |
        "coop.mw" |
        "edu.mw" |
        "com.mw" |
        "ac.mw" |
        "int.mw" |

        // Malaysia
        "net.my" |
        "mil.my" |
        "name.my" |
        "org.my" |
        "gov.my" |
        "edu.my" |
        "com.my" |
        "sch.my" |

        // Maldives (Republic of Maldives)
        "museum.mv" |
        "biz.mv" |
        "mil.mv" |
        "net.mv" |
        "pro.mv" |
        "aero.mv" |
        "name.mv" |
        "org.mv" |
        "gov.mv" |
        "coop.mv" |
        "edu.mv" |
        "info.mv" |
        "com.mv" |
        "int.mv" |

        // Mali
        "presse.ml" |
        "net.ml" |
        "org.ml" |
        "gov.ml" |
        "edu.ml" |
        "com.ml" |

        // Malta
        "net.mt" |
        "org.mt" |
        "gov.mt" |
        "edu.mt" |
        "com.mt" |

        // Mauritania
        "gov.mr" |

        // Mauritius
        "net.mu" |
        "co.mu" |
        "org.mu" |
        "gov.mu" |
        "or.mu" |
        "com.mu" |
        "ac.mu" |

        // Mexico
        "net.mx" |
        "org.mx" |
        "edu.mx" |
        "com.mx" |
        "gob.mx" |

        // Monaco
        "tm.mc" |
        "asso.mc" |

        // Mongolia
        "gov.mn" |
        "edu.mn" |
        "org.mn" |

        // Montenegro
        "priv.me" |
        "net.me" |
        "co.me" |
        "org.me" |
        "gov.me" |
        "edu.me" |
        "its.me" |
        "ac.me" |

        // Morocco
        "press.ma" |
        "net.ma" |
        "co.ma" |
        "org.ma" |
        "gov.ma" |
        "ac.ma" |

        // Mozambique
        "gov.mz" |
        "edu.mz" |
        "org.mz" |
        "co.mz" |

        // Namibia
        "co.na" |
        "ws.na" |
        "org.na" |
        "mobi.na" |
        "edu.na" |
        "alt.na" |
        "info.na" |
        "com.na" |
        "in.na" |

        // Nauru
        "biz.nr" |
        "net.nr" |
        "org.nr" |
        "gov.nr" |
        "edu.nr" |
        "info.nr" |
        "com.nr" |

        // Nepal
        "net.np" |
        "mil.np" |
        "org.np" |
        "gov.np" |
        "edu.np" |
        "com.np" |

        // New Zealand
        "gen.nz" |
        "net.nz" |
        "co.nz" |
        "mil.nz" |
        "govt.nz" |
        "cri.nz" |
        "org.nz" |
        "school.nz" |
        "ac.nz" |
        "iwi.nz" |
        "geek.nz" |
        "parliament.nz" |
        "maori.nz" |

        // Nicaragua
        "net.ni" |
        "co.ni" |
        "mil.ni" |
        "nom.ni" |
        "org.ni" |
        "ac.ni" |
        "gob.ni" |

        // Nigeria
        "net.ng" |
        "org.ng" |
        "gov.ng" |
        "edu.ng" |
        "com.ng" |

        // Norfolk Island
        "per.nf" |
        "net.nf" |
        "firm.nf" |
        "rec.nf" |
        "arts.nf" |
        "info.nf" |
        "store.nf" |
        "com.nf" |
        "web.nf" |
        "other.nf" |

        // Oman
        "museum.om" |
        "biz.om" |
        "net.om" |
        "co.om" |
        "mil.om" |
        "pro.om" |
        "org.om" |
        "gov.om" |
        "edu.om" |
        "med.om" |
        "com.om" |
        "sch.om" |
        "ac.om" |

        // Pakistan
        "com.pk" |  // added by @dylanburati
        "fam.pk" |
        "gop.pk" |
        "biz.pk" |
        "net.pk" |
        "gok.pk" |
        "org.pk" |
        "gon.pk" |
        "gov.pk" |
        "edu.pk" |
        "web.pk" |
        "gos.pk" |
        "gob.pk" |

        // Palau
        "net.pw" |
        "belau.pw" |
        "org.pw" |
        "gov.pw" |
        "edu.pw" |
        "com.pw" |

        // Palestinian Territory | Occupied
        "biz.ps" |
        "net.ps" |
        "gov.ps" |
        "edu.ps" |
        "com.ps" |
        "sch.ps" |
        "mun.ps" |

        // Panama
        "sld.pa" |
        "net.pa" |
        "nom.pa" |
        "org.pa" |
        "ing.pa" |
        "edu.pa" |
        "med.pa" |
        "com.pa" |
        "ac.pa" |
        "gob.pa" |
        "abo.pa" |

        // Papua New Guinea
        "net.pg" |
        "mil.pg" |
        "org.pg" |
        "gov.pg" |
        "com.pg" |
        "ac.pg" |

        // Paraguay
        "mil.py" |
        "net.py" |
        "org.py" |
        "gov.py" |
        "edu.py" |
        "com.py" |
        "una.py" |

        // Peru
        "sld.pe" |
        "mil.pe" |
        "net.pe" |
        "nom.pe" |
        "org.pe" |
        "edu.pe" |
        "com.pe" |
        "gob.pe" |

        // Philippines
        "mil.ph" |
        "net.ph" |
        "org.ph" |
        "gov.ph" |
        "edu.ph" |
        "ngo.ph" |
        "i.ph" |
        "com.ph" |

        // Pitcairn Islands
        "gov.pl" |
        "com.pl" |
        "org.pl" |

        // Poland
        "biz.pl" |
        "warszawa.pl" |
        "wroclaw.pl" |
        "lublin.pl" |
        "ngo.pl" |
        "info.pl" |
        "torun.pl" |
        "radom.pl" |
        // "org.pl" |
        "gdansk.pl" |
        "gda.pl" |
        "edu.pl" |
        "poznan.pl" |
        // "com.pl" |
        "bialystok.pl" |
        "mil.pl" |
        "katowice.pl" |
        "zgora.pl" |
        "lodz.pl" |
        // "gov.pl" |
        "olsztyn.pl" |
        "krakow.pl" |
        "slupsk.pl" |
        "art.pl" |
        "wroc.pl" |
        "net.pl" |
        "gorzow.pl" |
        "szczecin.pl" |
        "waw.pl" |

        // Portugal
        "nome.pt" |
        "net.pt" |
        "publ.pt" |
        "org.pt" |
        "gov.pt" |
        "edu.pt" |
        "com.pt" |
        "int.pt" |

        // Puerto Rico
        "biz.pr" |
        "net.pr" |
        "pro.pr" |
        "name.pr" |
        "org.pr" |
        "prof.pr" |
        "gov.pr" |
        "edu.pr" |
        "info.pr" |
        "ac.pr" |
        "com.pr" |
        "est.pr" |
        "isla.pr" |

        // Qatar
        "net.qa" |
        "org.qa" |
        "gov.qa" |
        "edu.qa" |
        "com.qa" |

        // Romania
        "tm.ro" |
        "firm.ro" |
        "rec.ro" |
        "nt.ro" |
        "www.ro" |
        "nom.ro" |
        "org.ro" |
        "info.ro" |
        "store.ro" |
        "com.ro" |
        "arts.ro" |

        // Réunion
        "com.re" |
        "asso.re" |
        "nom.re" |

        // Rwanda
        "gouv.rw" |
        "net.rw" |
        "co.rw" |
        "mil.rw" |
        "gov.rw" |
        "edu.rw" |
        "com.rw" |
        "ac.rw" |
        "int.rw" |

        // Saint Helena
        "net.sh" |
        "co.sh" |
        "nom.sh" |
        "org.sh" |
        "gov.sh" |
        "edu.sh" |
        "com.sh" |

        // Saint Kitts and Nevis
        "gov.kn" |
        "net.kn" |
        "edu.kn" |
        "org.kn" |

        // Saint Lucia
        "net.lc" |
        "co.lc" |
        "org.lc" |
        "p.lc" |
        "l.lc" |
        "com.lc" |

        // Saint Vincent and the Grenadines
        "com.vc" |
        "net.vc" |
        "org.vc" |

        // Samoa
        "net.ws" |
        "org.ws" |
        "gov.ws" |
        "edu.ws" |
        "com.ws" |

        // São Tomé and Príncipe
        "consulado.st" |
        "net.st" |
        "mil.st" |
        "co.st" |
        "principe.st" |
        "saotome.st" |
        "org.st" |
        "gov.st" |
        "edu.st" |
        "store.st" |
        "com.st" |
        "embaixada.st" |

        // Saudi Arabia
        "net.sa" |
        "org.sa" |
        "gov.sa" |
        "edu.sa" |
        "med.sa" |
        "com.sa" |
        "pub.sa" |
        "sch.sa" |

        // Serbia
        "co.rs" |
        "org.rs" |
        "in.rs" |
        "gov.rs" |
        "edu.rs" |
        "ac.rs" |

        // Seychelles
        "net.sc" |
        "org.sc" |
        "gov.sc" |
        "edu.sc" |
        "com.sc" |

        // Sierra Leone
        "net.sl" |
        "org.sl" |
        "gov.sl" |
        "edu.sl" |
        "com.sl" |

        // Singapore
        "per.sg" |
        "net.sg" |
        "org.sg" |
        "gov.sg" |
        "edu.sg" |
        "idn.sg" |
        "com.sg" |

        // Solomon Islands
        "net.sb" |
        "org.sb" |
        "gov.sb" |
        "edu.sb" |
        "com.sb" |

        // Spain
        "nom.es" |
        "org.es" |
        "edu.es" |
        "com.es" |
        "gob.es" |

        // Sri Lanka
        "hotel.lk" |
        "assn.lk" |
        "net.lk" |
        "org.lk" |
        "gov.lk" |
        "grp.lk" |
        "edu.lk" |
        "ngo.lk" |
        "ltd.lk" |
        "soc.lk" |
        "com.lk" |
        "sch.lk" |
        "web.lk" |
        "int.lk" |

        // Sudan
        "net.sd" |
        "org.sd" |
        "tv.sd" |
        "gov.sd" |
        "edu.sd" |
        "info.sd" |
        "med.sd" |
        "com.sd" |

        // Swaziland
        "ac.sz" |
        "org.sz" |
        "co.sz" |

        // Sweden
        "z.se" |
        "tm.se" |
        "bd.se" |
        "m.se" |
        "l.se" |
        "r.se" |
        "k.se" |
        "o.se" |
        "f.se" |
        "a.se" |
        "e.se" |
        "org.se" |
        "pp.se" |
        "n.se" |
        "d.se" |
        "h.se" |
        "t.se" |
        "g.se" |
        "p.se" |
        "s.se" |
        "b.se" |
        "x.se" |
        "press.se" |
        "c.se" |
        "u.se" |
        "parti.se" |
        "i.se" |
        "y.se" |
        "ac.se" |
        "w.se" |

        // Syria
        "net.sy" |
        "mil.sy" |
        "org.sy" |
        "gov.sy" |
        "news.sy" |
        "edu.sy" |
        "com.sy" |

        // Taiwan (Republic of China))
        "game.tw" |
        "mil.tw" |
        "net.tw" |
        "idv.tw" |
        "club.tw" |
        "org.tw" |
        "gov.tw" |
        "edu.tw" |
        "com.tw" |
        "ebiz.tw" |

        // Tajikistan
        "go.tj" |
        "biz.tj" |
        "aero.tj" |
        "info.tj" |
        "co.tj" |
        "dyn.tj" |
        "org.tj" |
        "edu.tj" |
        "coop.tj" |
        "com.tj" |
        "museum.tj" |
        "mil.tj" |
        "pro.tj" |
        "name.tj" |
        "gov.tj" |
        "my.tj" |
        "web.tj" |
        "per.tj" |
        "net.tj" |
        "ac.tj" |
        "int.tj" |

        // Tanzania
        "go.tz" |
        "co.tz" |
        "ne.tz" |
        "or.tz" |
        "ac.tz" |

        // Thailand
        "go.th" |
        "net.th" |
        "co.th" |
        "in.th" |
        "mi.th" |
        "or.th" |
        "ac.th" |

        // Trinidad and Tobago
        "biz.tt" |
        "net.tt" |
        "co.tt" |
        "pro.tt" |
        "name.tt" |
        "org.tt" |
        "gov.tt" |
        "edu.tt" |
        "info.tt" |
        "com.tt" |

        // Tunisia
        "rns.tn" |
        "defense.tn" |
        "rnu.tn" |
        "ens.tn" |
        "info.tn" |
        "rnrt.tn" |
        "org.tn" |
        "mincom.tn" |
        "com.tn" |
        "edunet.tn" |
        "ind.tn" |
        "fin.tn" |
        "intl.tn" |
        "gov.tn" |
        "net.tn" |
        "tourism.tn" |
        "perso.tn" |
        "agrinet.tn" |
        "nat.tn" |

        // Turkey
        "biz.tr" |
        "k12.tr" |
        "info.tr" |
        "gen.tr" |
        "dr.tr" |
        "bbs.tr" |
        "org.tr" |
        "tv.tr" |
        "edu.tr" |
        "com.tr" |
        "bel.tr" |
        "mil.tr" |
        "tel.tr" |
        "name.tr" |
        "gov.tr" |
        "web.tr" |
        "av.tr" |
        "net.tr" |
        "tsk.tr" |
        "pol.tr" |

        // Uganda
        "go.ug" |
        "co.ug" |
        "ne.ug" |
        "sc.ug" |
        "or.ug" |
        "ac.ug" |

        // Ukraine
        "net.ua" |
        "org.ua" |
        "gov.ua" |
        "edu.ua" |
        "com.ua" |
        "in.ua" |

        // United Arab Emirates
        "net.ae" |
        "co.ae" |
        "mil.ae" |
        "pro.ae" |
        "name.ae" |
        "org.ae" |
        "gov.ae" |
        "sch.ae" |
        "ac.ae" |

        // United Kingdom
        "nhs.uk" |
        "net.uk" |
        "co.uk" |
        "mod.uk" |
        "org.uk" |
        "gov.uk" |
        "sch.uk" |
        "nic.uk" |
        "ltd.uk" |
        "me.uk" |
        "police.uk" |
        "ac.uk" |
        "plc.uk" |

        // United States of America
        "ny.us" |
        "fed.us" |
        "id.us" |
        "nh.us" |
        "ok.us" |
        "wa.us" |
        "hi.us" |
        "ri.us" |
        "mi.us" |
        "or.us" |
        "tn.us" |
        "md.us" |
        "gu.us" |
        "nsn.us" |
        "co.us" |
        "ne.us" |
        "oh.us" |
        "ak.us" |
        "nd.us" |
        "tx.us" |
        "wi.us" |
        "ga.us" |
        "isa.us" |
        "il.us" |
        "ct.us" |
        "sd.us" |
        "ia.us" |
        "mt.us" |
        "al.us" |
        "pa.us" |
        "az.us" |
        "sc.us" |
        "ky.us" |
        "ma.us" |
        "nj.us" |
        "wv.us" |
        "dni.us" |
        "nc.us" |
        "ar.us" |
        "fl.us" |
        "ca.us" |
        "ut.us" |
        "wy.us" |
        "vi.us" |
        "in.us" |
        "kids.us" |
        "pr.us" |
        "dc.us" |
        "de.us" |
        "mn.us" |
        "ks.us" |
        "nm.us" |
        "vt.us" |
        "la.us" |
        "mo.us" |
        "va.us" |
        "mp.us" |
        "me.us" |
        "nv.us" |
        "ms.us" |
        "as.us" |

        // Uruguay
        "mil.uy" |
        "net.uy" |
        "org.uy" |
        "gub.uy" |
        "edu.uy" |
        "com.uy" |

        // Uds
        "com.vi" |
        "co.vi" |

        // Uzbekistan
        "com.uz" |
        "co.uz" |

        // Venezuela
        "net.ve" |
        "mil.ve" |
        "co.ve" |
        "org.ve" |
        "edu.ve" |
        "info.ve" |
        "com.ve" |
        "web.ve" |
        "gob.ve" |

        // Vietnam
        "biz.vn" |
        "net.vn" |
        "pro.vn" |
        "name.vn" |
        "org.vn" |
        "gov.vn" |
        "edu.vn" |
        "info.vn" |
        "health.vn" |
        "com.vn" |
        "ac.vn" |
        "int.vn" |

        // Yemen
        "me.ye" |
        "net.ye" |
        "co.ye" |
        "org.ye" |
        "gov.ye" |
        "ltd.ye" |
        "com.ye" |
        "plc.ye" |

        // Zambia
        "com.zm" |
        "ac.zm" |
        "org.zm" |
        "co.zm" |

        // Zimbabwe
        "co.zw" |

        // Russia: https://cctld.ru/en/domains/domens_ru/reserved.php
        "krasnoyarsk.ru" |
        "buryatia.ru" |
        "astrakhan.ru" |
        "smolensk.ru" |
        "msk.ru" |
        "ptz.ru" |
        "mari-el.ru" |
        "vdonsk.ru" |
        "koenig.ru" |
        "jamal.ru" |
        "sakhalin.ru" |
        "jar.ru" |
        "com.ru" |
        "yamal.ru" |
        "saratov.ru" |
        "tver.ru" |
        "zgrad.ru" |
        "rnd.ru" |
        "kuzbass.ru" |
        "vrn.ru" |
        "spb.ru" |
        "nakhodka.ru" |
        "e-burg.ru" |
        "chelyabinsk.ru" |
        "murmansk.ru" |
        "cbg.ru" |
        "pskov.ru" |
        "vladimir.ru" |
        "kemerovo.ru" |
        "izhevsk.ru" |
        "tomsk.ru" |
        "nkz.ru" |
        "vladivostok.ru" |
        "samara.ru" |
        "khakassia.ru" |
        "vologda.ru" |
        "ulan-ude.ru" |
        "amur.ru" |
        "snz.ru" |
        "oryol.ru" |
        "mosreg.ru" |
        "perm.ru" |
        "lipetsk.ru" |
        "adygeya.ru" |
        "udm.ru" |
        "altai.ru" |
        "volgograd.ru" |
        "tyumen.ru" |
        "surgut.ru" |
        "kchr.ru" |
        "belgorod.ru" |
        "kursk.ru" |
        "dagestan.ru" |
        "yakutia.ru" |
        "kaluga.ru" |
        "udmurtia.ru" |
        "komi.ru" |
        "pp.ru" |
        "chuvashia.ru" |
        "edu.ru" |
        "chita.ru" |
        "vladikavkaz.ru" |
        "int.ru" |
        "test.ru" |
        "nov.ru" |
        "kustanai.ru" |
        "kirov.ru" |
        "ivanovo.ru" |
        "bir.ru" |
        "tom.ru" |
        "pyatigorsk.ru" |
        "oskol.ru" |
        "kms.ru" |
        "yaroslavl.ru" |
        "fareast.ru" |
        "penza.ru" |
        "khabarovsk.ru" |
        "ryazan.ru" |
        "magadan.ru" |
        "bryansk.ru" |
        "magnitka.ru" |
        "nnov.ru" |
        "palana.ru" |
        "stv.ru" |
        "kamchatka.ru" |
        "amursk.ru" |
        "tambov.ru" |
        "kurgan.ru" |
        "norilsk.ru" |
        "k-uralsk.ru" |
        "yekaterinburg.ru" |
        "bashkiria.ru" |
        "dudinka.ru" |
        "baikal.ru" |
        "mordovia.ru" |
        "mytis.ru" |
        "kazan.ru" |
        "tsk.ru" |
        "irkutsk.ru" |
        "omsk.ru" |
        "tula.ru" |
        "vyatka.ru" |
        "chukotka.ru" |
        "ac.ru" |
        "cmw.ru" |
        "tsaritsyn.ru" |
        "simbirsk.ru" |
        "nsk.ru" |
        "syzran.ru" |
        "rubtsovsk.ru" |
        "stavropol.ru" |
        "khv.ru" |
        "chel.ru" |
        "org.ru" |
        "novosibirsk.ru" |
        "yuzhno-sakhalinsk.ru" |
        "kuban.ru" |
        "kalmykia.ru" |
        "tatarstan.ru" |
        "orenburg.ru" |
        "mil.ru" |
        "grozny.ru" |
        "nalchik.ru" |
        "mari.ru" |
        "gov.ru" |
        "kostroma.ru" |
        "karelia.ru" |
        "voronezh.ru" |
        "net.ru" |
        "joshkar-ola.ru" |
        "marine.ru" |
        "tuva.ru" |
        "arkhangelsk.ru" |

        // South Africa: https://en.wikipedia.org/wiki/.za
        "tm.za" |
        "iaccess.za" |
        "db.za" |
        "inca.za" |
        "ngo.za" |
        "bourse.za" |
        "law.za" |
        "grondar.za" |
        "co.za" |
        "org.za" |
        "edu.za" |
        "alt.za" |
        "pix.za" |
        "landesign.za" |
        "olivetti.za" |
        "mil.za" |
        "nom.za" |
        "gov.za" |
        "web.za" |
        "city.za" |
        "cybernet.za" |
        "net.za" |
        "imt.za" |
        "agric.za" |
        "school.za" |
        "ac.za" |
        "nis.za" |

        // South Korea: https://en.wikipedia.org/wiki/.kr#Domains_and_Subdomains
        "go.kr" |
        "seoul.kr" |
        "gangwon.kr" |
        "jeonbuk.kr" |
        "or.kr" |
        "busan.kr" |
        "pe.kr" |
        "kg.kr" |
        "re.kr" |
        "co.kr" |
        "ne.kr" |
        "gwangju.kr" |
        "es.kr" |
        "ac.kr" |
        "gyeongbuk.kr" |
        "mil.kr" |
        "daejeon.kr" |
        "gyeonggi.kr" |
        "sc.kr" |
        "hs.kr" |
        // "한글.kr" |
        "chungbuk.kr" |
        "incheon.kr" |
        "jeonnam.kr" |
        "gyeongnam.kr" |
        "ulsan.kr" |
        "jeju.kr" |
        "ms.kr" |
        "daegu.kr" |
        "chungnam.kr"
    )
}
