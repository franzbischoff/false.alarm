# Guia de Migração: Pipeline Antiga → Nova Pipeline Modular

## Resumo das Mudanças

A pipeline antiga (`scripts/1_compute_all_scores.R`) foi refatorizada em 5 scripts modulares organizados em 2 fases distintas, com outputs intermédios estruturados e suporte para grid search expandido.

## Comparação Estrutural

### Pipeline Antiga (Monolítica)
```
scripts/1_compute_all_scores.R
├─ Carrega dados
├─ Computa FLOSS para todos window_sizes
├─ Gera predições
├─ Calcula scores
└─ Adiciona baselines
    └─ Output: output/afib_regimes.rds (único ficheiro)
```

### Nova Pipeline (Modular)
```
scripts/regime_detection/
├─ 00_master_pipeline.R (orquestração)
├─ config.R (configuração centralizada)
├─ 10_prepare_data.R
│   └─ Output: generation/tidy_dataset.rds
├─ 20_generate_matrix_profiles.R
│   └─ Output: generation/matrix_profiles_w{25..400}.rds
├─ 30_predict_grid_search.R
│   └─ Output: prediction/predictions_grid.rds
├─ 40_compute_scores.R
│   └─ Output: prediction/final_scores.rds
└─ 50_convert_to_csv.R
    └─ Output: {dataset}_predictions.csv
```

## Principais Melhorias

### 1. **Separação de Fases Computacionais**

**Antes**: Matrix Profile e predições calculados em sequência, tudo em memória.

**Depois**:
- **FASE 1 (Generation)**: Computa MPs uma vez, salva em disco
- **FASE 2 (Prediction)**: Carrega MPs conforme necessário, otimiza memória

**Vantagem**: Não precisa recomputar MPs (parte mais lenta) para testar diferentes hiperparâmetros de predição.

### 2. **Gestão de Memória**

**Antes**:
```r
# Carrega TODOS os FLOSS para TODOS os window_sizes em memória
floss_dataset <- NULL
for (w in var_window_size) {
  floss <- furrr::future_map(tidy_dataset$ts, ~ floss_train_regimes(...))
  floss_dataset <- bind_rows(floss_dataset, ...)
}
```

**Depois**:
```r
# Script 20: Computa e salva UM window_size de cada vez
for (w in var_window_size) {
  floss <- furrr::future_map(...)
  saveRDS(floss, "matrix_profiles_w{w}.rds")
  rm(floss); gc()  # Liberta memória
}

# Script 30: Carrega MPs em batches
for (batch in window_batches) {
  mp_dataset <- readRDS("matrix_profiles_w{w}.rds")
  # ... processa ...
  rm(mp_dataset); gc()
}
```

**Vantagem**: Menor consumo de memória RAM, permite processar datasets maiores.

### 3. **Grid Search Expandido**

**Antes**:
```r
# min_gap_samples FIXO
pred <- floss_predict(...) |> clean_pred(200, FALSE)

# Grid: window_size × regime_threshold × regime_landmark
# 16 × 18 × 15 = 4,320 combinações
```

**Depois**:
```r
# min_gap_samples VARIÁVEL
raw_pred <- floss_predict(...)
for (min_gap in c(1000, 2000, 3000, 5000)) {
  pred <- clean_pred(raw_pred, min_gap, FALSE)
}

# Grid: window_size × regime_threshold × regime_landmark × min_gap_samples
# 16 × 18 × 15 × 4 = 17,280 combinações
```

**Vantagem**:
- Comparabilidade com projeto Python (que testou estes valores)
- Análise de sensibilidade do parâmetro de supressão de duplicados

### 4. **Checkpoints e Recuperação**

**Antes**: Se falhar a meio, recomeçar tudo do zero.

**Depois**:
- Script 20 verifica se `matrix_profiles_w{size}.rds` existe antes de computar
- Cada script salva outputs intermédios
- Pode recomeçar do último script bem-sucedido

### 5. **Configuração Centralizada**

**Antes**: Parâmetros espalhados pelo código.

**Depois**: `config.R` centraliza todas as configurações:
```r
source("scripts/regime_detection/config.R")
# Todas as variáveis disponíveis: DATASET_NAME, VAR_WINDOW_SIZE, etc.
```

**Vantagem**:
- Mudança de dataset requer edição de 1 ficheiro
- Consistência garantida entre scripts

### 6. **Estrutura de Outputs Organizada**

**Antes**:
```
output/
├─ afib_regimes.rds
├─ afib_regimes-1.rds
├─ afib_regimes-2.rds
└─ ... (ficheiros misturados)
```

**Depois**:
```
output/regime_detection/afib_regimes/
├─ generation/
│   ├─ tidy_dataset.rds
│   ├─ matrix_profiles_w25.rds
│   └─ ... (16 MPs)
├─ prediction/
│   ├─ predictions_grid.rds
│   └─ final_scores.rds
└─ afib_regimes_predictions.csv
```

**Vantagem**:
- Clara separação de fases
- Fácil identificar outputs intermédios
- Suporta múltiplos datasets sem conflitos

## Equivalência de Funcionalidades

| Funcionalidade | Pipeline Antiga | Nova Pipeline |
|----------------|-----------------|---------------|
| Carregamento ECG | ✅ `read_and_prepare_ecgs()` | ✅ Script 10 |
| Computação MP | ✅ Loop `for (w in var_window_size)` | ✅ Script 20 |
| Predições | ✅ `floss_predict()` + `clean_pred(200)` | ✅ Script 30 (4 valores min_gap) |
| Scoring | ✅ `score_pr()` | ✅ Script 40 |
| Baselines | ✅ 1s e 4s | ✅ Script 40 |
| Exportação CSV | ❌ (script separado) | ✅ Script 50 (integrado) |

## Estrutura de Dados

### RDS Antigo vs. Novo

**`output/afib_regimes.rds`** (antigo):
```r
# Colunas: record, truth, window_size, regime_threshold, regime_landmark,
#          pred, score, length, baseline, baseline_score, baseline4, baseline4_score
# 989,280 rows = 229 records × 4,320 combinations
# min_gap_samples FIXO em 200
```

**`output/regime_detection/afib_regimes/prediction/final_scores.rds`** (novo):
```r
# Colunas: record, truth, window_size, regime_threshold, regime_landmark,
#          min_gap_samples, pred, score, length,
#          baseline_1s, baseline_1s_score, baseline_4s, baseline_4s_score
# 3,957,120 rows = 229 records × 17,280 combinations
# min_gap_samples VARIÁVEL: [1000, 2000, 3000, 5000]
```

### CSV Antigo vs. Novo

**`output/afib_regimes_predictions_intermediate.csv`** (antigo):
```csv
record_id,detector,window_size,regime_threshold,regime_landmark,min_gap_samples,...
# min_gap_samples = 200 (fixo)
```

**`output/regime_detection/afib_regimes/afib_regimes_predictions.csv`** (novo):
```csv
record_id,detector,window_size,regime_threshold,regime_landmark,min_gap_samples,...
# min_gap_samples ∈ {1000, 2000, 3000, 5000}
```

## Como Migrar Análises Existentes

### Se estava a usar `output/afib_regimes.rds`:

**Opção 1 - Usar nova pipeline completa**:
```r
# Executar nova pipeline
source("scripts/regime_detection/10_prepare_data.R")
source("scripts/regime_detection/20_generate_matrix_profiles.R")
source("scripts/regime_detection/30_predict_grid_search.R")
source("scripts/regime_detection/40_compute_scores.R")

# Carregar resultados
data <- readRDS("output/regime_detection/afib_regimes/prediction/final_scores.rds")

# Filtrar para min_gap_samples = 200 (equivalente ao antigo)
# NOTA: 200 não está no grid novo. Usar 1000 (mais próximo) ou recomputar.
```

**Opção 2 - Continuar com pipeline antiga** (não recomendado):
```r
# Pipeline antiga ainda funcional em scripts/1_compute_all_scores.R
# Mas não terá min_gap_samples variável nem estrutura organizada
```

### Se estava a ler CSV para Python:

**Atualizar caminho**:
```python
# Antes
df = pd.read_csv("output/afib_regimes_predictions_intermediate.csv")

# Depois
df = pd.read_csv("output/regime_detection/afib_regimes/afib_regimes_predictions.csv")
```

**Atenção**:
- CSV novo tem 4× mais linhas (4 valores de min_gap_samples)
- Filtrar `min_gap_samples == 1000` para comparar com baseline Python

## Recomendações de Uso

### Para Desenvolvimento/Teste
1. Editar `config.R`: `VAR_LIMIT_PER_CLASS <- 10`
2. Executar: `source("scripts/regime_detection/00_master_pipeline.R")`
3. Verificar outputs em `output/regime_detection/afib_regimes/`

### Para Produção (Dataset Completo)
1. Editar `config.R`: `VAR_LIMIT_PER_CLASS <- NULL`
2. Ajustar `N_WORKERS` conforme CPU/RAM disponível
3. Executar scripts sequencialmente (10 → 20 → 30 → 40 → 50)
4. Monitorizar memória durante Script 20 (mais intensivo)

### Para Re-executar Grid Search
Se já tem MPs computados (Script 20 completo):
1. Editar hiperparâmetros em `config.R`
2. Executar apenas Scripts 30 → 40 → 50
3. MPs serão reutilizados (economiza horas de computação)

## Ficheiros Obsoletos

Após migração completa, considere arquivar:
- `scripts/1_compute_all_scores.R` (substituído pela nova pipeline)
- `scripts/3_convert_to_csv.R` (substituído por `50_convert_to_csv.R`)
- `output/afib_regimes.rds` (formato antigo)
- `output/afib_regimes-*.rds` (splits parciais)
- `output/*_predictions_intermediate.csv` (formato antigo)

## Questões Frequentes

**Q: Posso usar a pipeline antiga e nova em paralelo?**
A: Sim, os outputs vão para diretórios diferentes. Mas recomenda-se migrar completamente.

**Q: Como recomputar apenas um window_size?**
A: Apagar `output/regime_detection/{dataset}/generation/matrix_profiles_w{size}.rds` e executar Script 20.

**Q: Os resultados são idênticos?**
A: Para `min_gap_samples=200`, os algoritmos são idênticos. Mas 200 não está no grid novo (usa 1000, 2000, 3000, 5000).

**Q: Qual o tempo estimado de execução?**
A:
- Script 10: ~2 seg (10 ficheiros) / ~30 seg (229 ficheiros)
- Script 20: ~30 min (10 ficheiros, 10 workers) / ~8-12 horas (229 ficheiros)
- Script 30: ~1-2 horas (10 ficheiros) / ~24-48 horas (229 ficheiros)
- Script 40: ~30 min
- Script 50: ~5 min

**Q: Posso paralelizar mais?**
A: Script 20 já paraleliza com `future::multicore`. Aumentar `N_WORKERS` se tiver RAM.

## Contacto

Para questões ou bugs, contactar o autor da tese ou abrir issue no repositório.
