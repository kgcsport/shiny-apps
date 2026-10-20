export function calculateTaxIncidence({ demandSlope, supplySlope, tax }) {
  const b = Number(demandSlope);
  const d = Number(supplySlope);
  const t = Number(tax);
  if (!(b > 0) || !(d > 0) || !(t >= 0)) throw new RangeError("Slopes must be positive and tax cannot be negative.");
  const qStar = 6;
  const pStar = 8;
  const a = pStar + qStar * b;
  const c = pStar - qStar * d;
  const qTax = Math.max(0, (a - c - t) / (b + d));
  const sellerPrice = c + d * qTax;
  const buyerPrice = sellerPrice + t;
  const buyerBurden = buyerPrice - pStar;
  const sellerBurden = pStar - sellerPrice;
  return {
    a, b, c, d, tax:t, qStar, pStar, qTax, buyerPrice, sellerPrice,
    buyerBurden, sellerBurden, buyerShare: b / (b + d), sellerShare: d / (b + d),
    revenue: t * qTax, deadweightLoss: 0.5 * t * (qStar - qTax)
  };
}

export function taxIncidenceCsv(values) {
  const result = calculateTaxIncidence(values);
  const headers = ["demand_slope", "supply_slope", "tax_per_unit", "pre_tax_quantity", "post_tax_quantity", "buyer_price", "seller_price", "tax_revenue", "deadweight_loss"];
  const row = [result.b, result.d, result.tax, result.qStar, result.qTax, result.buyerPrice, result.sellerPrice, result.revenue, result.deadweightLoss];
  return `${headers.join(",")}\n${row.join(",")}\n`;
}
