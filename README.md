# Real-Time Stock Analyzer (Haskell)

Ova aplikacija je jednostavan alat za analizu burzovnih dionica u stvarnom vremenu, napisana u funkcijskom stilu koristeći programski jezik Haskell.

## 🎯 Cilj projekta

- Prikaz funkcijskog pristupa u Haskellu uz jasno odvajanje čistih i nečistih funkcija (`IO`)
- Analiza cijena dionica (npr. AAPL, GOOGL, MSFT) korištenjem AlphaVantage API-ja
- Vizualizacija cijene i SMA-10 (Simple Moving Average) na linijskom grafu

## 🧱 Struktura projekta

- **Main.hs** – Glavni program: pokreće petlju za dohvat podataka, obradu i crtanje grafova
- **StockData.hs** – Sadrži:
  - Definicije tipova (TimeSeries, SMA, itd.)
  - Čiste funkcije za parsiranje i obradu podataka
  - `IO` funkcije izolirane za mrežni dohvat i ispis grafa

## 🛠️ Kako pokrenuti

1. Instaliraj potrebne biblioteke:
   ```
   cabal update
   cabal build
   ```

2. Pokreni aplikaciju:
   ```
   cabal run stock-analyzer-cabal
   ```

3. Rezultat:
   - Svakih 5 minuta dohvaćaju se podaci za zadane dionice
   - Ispisuje se status obrade u terminalu
   - Generira se SVG graf `combined_chart.svg` s cijenama i SMA-10 linijama

## ✅ Tehnički detalji

- API: [AlphaVantage](https://www.alphavantage.co/)
- JSON parsiranje: `aeson`
- Grafovi: `Chart`, `Chart-diagrams`, `diagrams-svg`
- Dohvat podataka: `http-client`, `http-client-tls`

## 💡 Napomene

- Kod je pisan u funkcijskom stilu: čiste funkcije su jasno odvojene od `IO`
- Greške se obrađuju preko `Either`, ne koristi se `IO` unutar parsiranja
- Retry mehanizam za API rate limit je implementiran

---

© 2025 Erik Mirkovic | Fakultet informatike