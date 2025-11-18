# Regime Detection Pipeline

Este diretório contém a pipeline modular para deteção de mudanças de regime em sinais de ECG usando o algoritmo FLOSS (Fast Low-cost Online Semantic Segmentation) baseado em Matrix Profile.

## Estrutura da Pipeline

A pipeline está dividida em 4 scripts sequenciais organizados em 2 fases principais:

### FASE 1: GENERATION (Computação de Matrix Profiles)

Esta fase é computacionalmente intensiva mas precisa ser executada apenas uma vez para cada dataset.

#### Script 10: Preparação de Dados
**Ficheiro**: `10_prepare_data.R`

- Carrega ficheiros ECG do dataset
- Reamostra de 200Hz para 250Hz
- Normaliza sinais (z-score)
- Extrai ground truth (posições de mudanças de regime)
- **Output**: `output/regime_detection/{dataset}/generation/tidy_dataset.rds`

#### Script 20: Geração de Matrix Profiles
**Ficheiro**: `20_generate_matrix_profiles.R`

- Computa Matrix Profiles para window_sizes 25-400 (step 25 = 16 valores)
- Otimizado para memória: processa uma window_size de cada vez
- Usa paralelização com `future::multicore`
- **Output**: `output/regime_detection/{dataset}/generation/matrix_profiles_w{size}.rds` (16 ficheiros)

**Nota de Memória**:
- Configurado inicialmente com 10 ficheiros do dataset para teste
- Após otimização, processar os 229 ficheiros completos
- Python conseguiu 20 threads simultâneas com <30GB RAM

### FASE 2: PREDICTION (Grid Search Exaustivo)

Esta fase realiza o grid search sobre todos os hiperparâmetros.

#### Script 30: Grid Search de Predições
**Ficheiro**: `30_predict_grid_search.R`

Grid search sobre 4 dimensões:
- **window_size**: 16 valores (25 a 400, step 25)
- **regime_threshold**: 18 valores (0.05 a 0.9, step 0.05)
- **regime_landmark**: 15 valores (2s a 9s, step 0.5s) - atraso temporal
- **min_gap_samples**: 6 valores (200, 500, 1000, 2000, 3000, 5000 samples)

**Funcionamento**:
1. Carrega Matrix Profiles (um window_size de cada vez)
2. Para cada MP, aplica todas combinações de threshold × landmark
3. Gera predições RAW (sem clean_pred)
4. Aplica `clean_pred()` com cada valor de min_gap_samples
   - `clean_pred()` implementa timeout: mantém primeira detecção de cada grupo
   - Distância medida desde última detecção **aceite** (não consecutiva)
   - Remove automaticamente valores NA
5. **Output**: `output/regime_detection/{dataset}/prediction/predictions_grid.rds`

**Total de combinações por record**: 16 × 18 × 15 × 6 = **25,920**

**IMPORTANTE**: A avaliação (scoring) será feita no projeto Python, não em R.

#### Script 40: Conversão para CSV
**Ficheiro**: `40_convert_to_csv.R`

- Converte resultados para formato compatível com Python
- Transforma índices de samples para tempo em segundos
- Formata listas como JSON arrays
- **Output**: `output/regime_detection/{dataset}/{dataset}_predictions.csv`

**Formato CSV** (11 colunas):
- `record_id`: identificador do registo
- `detector`: "floss"
- `window_size`: tamanho da janela MP
- `regime_threshold`: threshold de deteção
- `regime_landmark`: atraso temporal (segundos)
- `min_gap_samples`: distância mínima entre predições
- `duration_seconds`: duração do sinal
- `gt_times`: ground truth em segundos (JSON array)
- `det_times`: deteções em segundos (JSON array)
- `n_detections`: número de deteções
- `n_ground_truth`: número de mudanças verdadeiras

## Como Usar

### Executar pipeline completa

```r
# Opção 1: Executar master script (visualizar estrutura)
source("scripts/regime_detection/00_master_pipeline.R")

# Opção 2: Executar cada script individualmente
source("scripts/regime_detection/10_prepare_data.R")
source("scripts/regime_detection/20_generate_matrix_profiles.R")
source("scripts/regime_detection/30_predict_grid_search.R")
source("scripts/regime_detection/40_convert_to_csv.R")

# Opção 3: Via terminal (recomendado para processamento completo)
Rscript scripts/regime_detection/10_prepare_data.R
Rscript scripts/regime_detection/20_generate_matrix_profiles.R
Rscript scripts/regime_detection/30_predict_grid_search.R
Rscript scripts/regime_detection/40_convert_to_csv.R
```

### Testar com subset

No **script 10**, altere:
```r
var_limit_per_class <- 10  # Apenas 10 ficheiros para teste
```

No **script 20**, configure workers:
```r
n_workers <- 10  # Começar com 10 threads
```

### Processar dataset completo

No **script 10**:
```r
var_limit_per_class <- NULL  # Processar todos os 229 ficheiros
```

No **script 20**:
```r
n_workers <- 20  # Aumentar paralelização se memória permitir
```

## Estrutura de Outputs

```
output/regime_detection/afib_regimes/
├── generation/
│   ├── tidy_dataset.rds                 # Dataset preparado
│   ├── matrix_profiles_w25.rds          # MP para window=25
│   ├── matrix_profiles_w50.rds          # MP para window=50
│   └── ... (16 ficheiros no total)
├── prediction/
│   └── predictions_grid.rds             # Todas predições (RDS)
└── afib_regimes_predictions.csv         # Formato Python (CSV)
```

## Hiperparâmetros

### Grid Search Completo

| Hiperparâmetro | Valores | Descrição |
|----------------|---------|-----------|
| `window_size` | 25-400 (step 25) | Tamanho da janela MP (16 valores) |
| `regime_threshold` | 0.05-0.9 (step 0.05) | Sensibilidade de deteção (18 valores) |
| `regime_landmark` | 2-9 (step 0.5) | Atraso temporal em segundos (15 valores) |
| `min_gap_samples` | [200, 500, 1000, 2000, 3000, 5000] | Timeout entre detecções (6 valores) |

**Total**: 25,920 combinações por record

### Interpretação

- **window_size**: Janela temporal do Matrix Profile (100ms a 1.6s @ 250Hz)
- **regime_threshold**: Quanto menor, mais sensível (mais deteções)
- **regime_landmark**: Atraso na deteção (streaming delay)
- **min_gap_samples**: Timeout entre detecções aceites (0.8s a 20s @ 250Hz)
  - Implementado via `clean_pred()`: mantém primeira detecção de cada grupo
  - Medido desde última detecção **aceite**, não consecutivas

## Comparação com Projeto Python

Este projeto R (FLOSS/Matrix Profile) será comparado com detectores Python:
- ADWIN
- KSWIN
- HDDM_A
- HDDM_W
- Page-Hinkley

**Repositório Python**: https://github.com/franzbischoff/ts-segmentation

**Métricas de avaliação comuns**:
- F1/F3 (weighted e classic)
- NAB (standard, low_fp, low_fn)
- Recall@4s, Recall@10s
- Precision@4s, Precision@10s
- FP/min, EDD Median

## Notas Importantes

1. **Memória**: Script 20 é o mais pesado. Ajuste `n_workers` conforme recursos disponíveis.

2. **Checkpoint**: Cada script salva outputs intermédios. Se falhar, pode recomeçar do último script bem-sucedido.

3. **Skip de ficheiros existentes**: Script 20 verifica se `matrix_profiles_w{size}.rds` já existe antes de recomputar.

4. **Sem scoring em R**: A avaliação (F1, F3, NAB, Recall, Precision, etc.) será feita no projeto Python para comparação direta com outros detectores.

5. **min_gap_samples**: Parâmetro adicionado ao grid search. Python testou [1000, 2000, 3000, 5000], aqui expandimos para [200, 500, 1000, 2000, 3000, 5000] para maior granularidade.

6. **clean_pred corrigido**: A função foi simplificada e corrigida para implementar corretamente o timeout desde a última detecção aceite (não mais baseada em diferenças consecutivas).

## Datasets Suportados

Atualmente configurado para `afib_regimes`, mas pode ser facilmente adaptado para:
- `vtachyarrhythmias` (sem resample, signal "ECG")
- `malignantventricular` (sem resample, signal "ECG1")

Para trocar de dataset, descomentar a linha correspondente nos scripts 10, 20, 30, 40:
```r
dataname <- "afib_regimes"          # ← Ativo
# dataname <- "vtachyarrhythmias"   # ← Descomentar para usar
# dataname <- "malignantventricular" # ← Descomentar para usar
```

No script 10, também ajustar as configurações específicas (já comentadas no ficheiro).
