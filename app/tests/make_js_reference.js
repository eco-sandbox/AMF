// Dump reference outputs of the v3 JavaScript model (amf_spectrum_app.html) for the R equivalence test.
const fs = require('fs'), path = require('path');
const root = path.join(__dirname, '..', '..');
const html = fs.readFileSync(path.join(root, 'amf_spectrum_app.html'), 'utf8');
const get = id => html.match(new RegExp('<script id="' + id + '">([\\s\\S]*?)<\\/script>'))[1];
const load = src => { const m = { exports: {} }; new Function('module', src)(m); return m.exports; };
const S1 = load(get('model')), M = load(get('model2'));
const out = {};
const pts = JSON.parse(fs.readFileSync(path.join(root, 'py_points.json'), 'utf8'));
out.static = pts.map(q => { const r = S1.model({ ...S1.DEFAULTS, S: q.S, L: q.L, kappa: q.kappa });
  return { S: q.S, L: q.L, kappa: q.kappa, py_mgr: q.mgr, py_cls: q.cls, MGR: r.MGR, dF: r.dF, T_eff: r.T_eff, B_AM: r.B_AM, B_NM: r.B_NM,
           benefit: r.benefit, costG: r.costG, limAM: r.limAM, limNM: r.limNM, cls: S1.classify(r.MGR, 0.05), cls2D: S1.classify2D(r.MGR, r.dF, 0.05) }; });
const T2 = { ...M.CH_DEFAULTS, c1: 1.65, c2: 0.6128, s: 0 };
out.table2 = [2, 3, 20].map(K => ({ K, final: M.simulate({ ...T2, K }, [1, 0.1, 0.1], 1500, 0.05).final }));
out.table1 = M.simulate({ ...T2, c1: 1.4, K: 20 }, [1, 0.1, 0.1], 3000, 0.05).final;
out.eq2 = [M.eq2(2, 5, 2, 0.3, 3, 1, 1), M.eq2(2, 0, 2, 0.4, 2, 1, 1)];
out.criteria = [2, 8, 40].map(K => { const c = M.criteria2({ ...M.DYN_DEFAULTS, K }); return { K, H: c.H, P: c.P, J12: c.J12, lnEq: c.lnEq, eqClass: c.eqClass, pulseClass: c.pulseClass, sink: c.sink, Kthr: c.Kthr }; });
const iv = M.invasion(T2); out.invasion = { QinvThr: iv.QinvThr, PinvThr: iv.PinvThr, Qinvades: iv.Qinvades, Pinvades: iv.Pinvades };
const SA = { ...M.CH_DEFAULTS, c1: 2.0, c2: 0.3, q0: 5 };
const share = p => { const f = M.simulate(p, [1, 0.1, 0.1], 400, 0.05, 2).final; return M.winner(f[1], f[2]); };
out.sneed = [5, 10, 20, 40].map(K => { for (let s = 0; s <= 1.0001; s += 0.01) if (share({ ...SA, K, s }) !== 'cheater') return Math.round(s * 100) / 100; return null; });
const dyn = M.simulate(M.as3(M.DYN_DEFAULTS), [1, 0.5, 0], 40, 0.01, 500);
out.dynTraj = { H: dyn.H, P: dyn.P, steps: dyn.steps };
const ch = M.simulate({ ...M.CH_DEFAULTS }, [1, 0.1, 0.1], 300, 0.05, 600);
out.cheatTraj = { H: ch.H, P: ch.P, Q: ch.Q, steps: ch.steps };
const SAD = { ...M.CH_DEFAULTS, K: 20, c1: 2.0, c2: 0.3, s: 0.3, q0: 5, T: 400 };
out.saMapCells = [];
for (const K of [1, 5.5, 20.5, 40]) for (const s of [0, 0.15, 0.3, 1]) { const f = M.simulate({ ...SAD, K, s }, [1, 0.1, 0.1], 300, 0.1, 2).final; out.saMapCells.push({ K, s, final: f }); }
out.chSweep = [0.5, 10.5, 40].map(K => ({ K, final: M.simulate({ ...M.CH_DEFAULTS, K }, [1, 0.1, 0.1], 400, 0.1, 5).final }));
fs.writeFileSync(path.join(__dirname, 'js_reference.json'), JSON.stringify(out));
console.log('sneed', out.sneed, 'table2', JSON.stringify(out.table2.map(x => x.final.map(v => +v.toFixed(4)))));
