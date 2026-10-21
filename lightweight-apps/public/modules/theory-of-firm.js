export function evaluateFirm({ A=80, B=2, F=0, a=40, b=-2, c=.02, d=.0005, qMax=100 }) {
  if (![A,B,F,a,b,c,d,qMax].every(Number.isFinite) || B<=0 || qMax<=0) throw new RangeError("Demand slope and quantity range must be positive.");
  const demand=q=>A-B*q, mr=q=>A-2*B*q, rawMc=q=>a+b*q+c*q*q+d*q*q*q, mc=q=>Math.max(0,rawMc(q));
  const tc=q=>F+a*q+b*q*q/2+c*q*q*q/3+d*q*q*q*q/4;
  const limit=Math.min(qMax,A/B), steps=2400; let q=0, profit=-F;
  for(let i=1;i<=steps;i++){const candidate=limit*i/steps, value=demand(candidate)*candidate-tc(candidate);if(value>profit){q=candidate;profit=value;}}
  const price=demand(q), totalRevenue=price*q, totalCost=tc(q), atc=q>0?totalCost/q:NaN;
  return { q, price, mr:mr(q), mcAtQ:mc(q), totalRevenue, totalCost, atc, profit:totalRevenue-totalCost, demand, mrCurve:mr, mc, tc };
}
export function firmCsv(parameters) { const e=evaluateFirm(parameters); return "quantity,price,marginal_revenue,marginal_cost,total_revenue,total_cost,average_total_cost,profit\n"+[e.q,e.price,e.mr,e.mc,e.totalRevenue,e.totalCost,e.atc,e.profit].join(",")+"\n"; }
