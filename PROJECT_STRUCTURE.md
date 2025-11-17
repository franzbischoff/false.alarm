# Estrutura do Projeto - False Alarm Detection

> Documentação da organização e arquitetura do projeto de doutoramento focado na deteção de mudanças de regime em sinais ECG usando Matrix Profile/FLOSS.

**Última atualização:** 17 de novembro de 2025

---

## Pipeline Principal

A pipeline atual foi simplificada, migrando do framework `targets` para scripts numerados que executam sequencialmente o processamento de dados.

```
scripts/
├── 1_compute_all_scores.R          # Script principal de computação (🔄 EM DESENVOLVIMENTO)
│   ├── Calcula Matrix Profile (MP) usando FLOSS
│   ├── Grid search de hiperparâmetros (window_size, regime_threshold, regime_landmark)
│   ├── Gera predições de mudanças de regime
│   ├── Calcula scores (F-score adaptado) comparando com ground truth
│   └── Adiciona baseline (detecções a cada 1s e 4s)
│
├── 2_cross_validation.R            # Cross-validation 5-folds (🔄 EM DESENVOLVIMENTO)
│   ├── Divide datasets em 5 folds
│   ├── Identifica top-3 modelos por fold
│   └── Calcula estatísticas (min, q25, median, q75, iqr, mean, max, sd)
│
├── common/                         # Funções auxiliares de processamento
│   ├── compute_floss.R             # Cálculo do FLOSS
│   ├── score_floss.R               # Métricas de avaliação
│   ├── read_ecg.R                  # Leitura de sinais ECG
│   ├── filter_data.R               # Filtros de sinal
│   ├── find_all_files.R            # Descoberta de ficheiros
│   ├── extract_regimes.R           # Extração de regimes
│   ├── compute_companion_stats.R
│   ├── compute_filters.R
│   ├── compute_s_profile_with_stats.R
│   ├── compute_streaming_profile.R
│   ├── extract_regime_sample.R
│   ├── fit_model.R
│   ├── get_set_attributes.R
│   ├── process_ts_in_file.R
│   ├── sqi.R
│   ├── utils_targets.R
│   └── validate_data.R
│
├── regimes/                        # Funções específicas de regime
│   ├── training_regimes.R
│   └── predict_regimes.R
│
├── helpers/                        # Utilitários gerais
│   └── glue_fmt.R
│
├── classification/                 # Pipeline de classificação (Fase 2)
│
├── _globals.R                      # Variáveis globais e configurações
├── _targets.R                      # Pipeline targets (DEPRECIADO)
├── _regime_change.R                # Experimento regime change (ARQUIVADO)
├── _regime_change2.R               # Variante experimento (ARQUIVADO)
├── _regime_change_holdout2.R       # Holdout set (ARQUIVADO)
├── _regime_optimize.R              # Otimização (ARQUIVADO)
├── _classifier.R                   # Pipeline classificação (FASE 2)
├── _contrast_profile.R             # Experimento contrast (ARQUIVADO)
└── _contrast_profile_ex.R          # Variante contrast (ARQUIVADO)
```

### Nota sobre Scripts com Underscore (`_`)
Scripts que começam com `_` (underscore) são ficheiros de configuração para pipelines do framework `targets`. Estes definem DAGs (Directed Acyclic Graphs) de processamento mas foram depreciados em favor dos scripts numerados (1_, 2_, etc.) para o workflow de grid search.

---

## Implementação Core (R/)

Funções principais implementadas em R/C++ (via Rcpp) para cálculo do Matrix Profile e FLOSS.

```
R/
├── floss_train.R                  # Treino do algoritmo FLOSS ✅
├── floss_predict.R                # Predição de mudanças de regime ✅
├── floss_data.R                   # Preparação de dados
├── floss_model.R                  # Definição do modelo
├── floss_tune.R                   # Tunning de hiperparâmetros
├── floss_yardstick.R              # Métricas customizadas
│
├── mpx.R                          # Matrix Profile core ✅
├── mass.R                         # MASS algorithm ✅
├── stomp.R                        # STOMP algorithm ✅
├── scrimp.R                       # SCRIMP algorithm ✅
├── stamp.R                        # STAMP algorithm ✅
├── windowfunc.R                   # Funções de janela ✅
│
├── contrast.R                     # Contrast Profile (experimental)
├── contrast_*.R                   # Variantes Contrast Profile
├── math.R                         # Utilitários matemáticos
├── utils-pipe.R                   # Pipe operators
└── false.alarm-package.R          # Documentação do package
```

**Implementação em C++:**
```
src/
├── mass.cpp                       # MASS (Mueen's Algorithm for Similarity Search)
├── stomp.cpp                      # STOMP (Scalable Time series Ordered-search Matrix Profile)
├── scrimp.cpp                     # SCRIMP (SCalable RandoM Pair)
├── stamp.cpp                      # STAMP (Scalable Time series Anytime Matrix Profile)
├── windowfunc.cpp                 # Funções de janela otimizadas
├── math.cpp                       # Operações matemáticas
└── RcppExports.cpp                # Bindings Rcpp automáticos
```

---

## Datasets Disponíveis

```
inst/extdata/
├── afib_regimes/                  # Dataset AFib (🎯 ATUAL - paroxysmal_afib)
│   ├── persistent_afib/           # AFib persistente
│   ├── paroxysmal_afib/           # AFib paroxismal (em uso)
│   └── non_afib/                  # Sem AFib
│
├── vtachyarrhythmias/             # Ventricular Tachyarrhythmias (✅ PROCESSADO)
│   └── CU Ventricular Tachyarrhythmia Database
│
├── malignantventricular/          # Malignant Ventricular (⏳ DISPONÍVEL)
│   └── MIT-BIH Malignant Ventricular Ectopy Database
│
├── arrhythmias/                   # MIT-BIH Arrhythmia (⏳ DISPONÍVEL)
│   └── MIT-BIH Arrhythmia Database
│
└── physionet/                     # Dados auxiliares PhysioNet
```

### Características dos Datasets

| Dataset | Frequência | Canais | Classes | Regimes | Status |
|---------|-----------|--------|---------|---------|--------|
| afib_regimes | 250 Hz | I, II | 3 tipos AFib | Sim | 🎯 Em uso |
| vtachyarrhythmias | 250 Hz | ECG | V-Tach | Sim | ✅ Processado |
| malignantventricular | 250 Hz | ECG1 | Ventricular | Sim | ⏳ Disponível |
| arrhythmias | 360 Hz | II | Multi-classe | Não* | ⏳ Disponível |

*Requer pré-processamento para identificar regimes

---

## Outputs Gerados

```
output/
├── afib_regimes-5.rds              # Resultados parciais (split 5)
├── afib_regimes.rds                # Dataset completo com scores (🔄 ATUAL)
│   ├── Estrutura:
│   │   ├── record              (ID do ficheiro)
│   │   ├── truth               (ground truth - regimes reais)
│   │   ├── window_size         (tamanho janela MP)
│   │   ├── regime_threshold    (limiar deteção FLOSS)
│   │   ├── regime_landmark     (landmark FLOSS)
│   │   ├── pred                (predições - regimes detetados)
│   │   ├── score               (F-score adaptado)
│   │   ├── baseline            (predições baseline 1s)
│   │   ├── baseline_score      (score baseline 1s)
│   │   ├── baseline4           (predições baseline 4s)
│   │   └── baseline4_score     (score baseline 4s)
│   │
│   └── Grid de hiperparâmetros:
│       ├── window_size: 350-400 (step 25) → 3 valores
│       ├── regime_threshold: 0.05-0.9 (step 0.05) → 18 valores
│       └── regime_landmark: 2-9 (step 0.5) → 15 valores
│       = 810 combinações por registo
│
├── vtachyarrhythmias.rds          # Dataset VT processado ✅
├── malignantventricular.rds       # Dataset MV (se processado)
│
├── *_cross_validation_results.rds # Resultados de CV (em desenvolvimento)
│
├── dbarts_fitted*.rds             # Modelos BART treinados (Fase 2)
├── importances*.rds               # Feature importances (Fase 2)
├── regime_outputs*.rds            # Outputs regime detection (experimentos)
├── scores_stats*.rds              # Estatísticas scores (análises)
│
└── README.md                      # Documentação mínima
```

---

## Diretórios de Trabalho (Metadados `targets`)

Estes diretórios contêm os metadados e resultados dos experimentos executados com o framework `targets`. Cada diretório representa uma pipeline diferente.

```
_targets/                          # Pipeline principal (DEPRECIADO)
│   ├── meta/                      # Metadados: DAG, dependências, timestamps
│   ├── objects/                   # Objetos R serializados (resultados dos nodes)
│   └── user/                      # Configurações do utilizador
│
_regime_change/                    # Experimento: deteção de mudanças (ARQUIVADO)
│   ├── meta/
│   ├── objects/
│   └── user/
│
_regime_change2/                   # Variante experimento regime (ARQUIVADO)
_regime_change3/                   # Outra variante (ARQUIVADO)
_regime_change_2/                  # Outra variante (ARQUIVADO)
│
_regime_optimize/                  # Otimização hiperparâmetros (ARQUIVADO)
│
_classifier/                       # Pipeline classificação - Fase 2 (⏸️ PAUSADO)
│   ├── meta/
│   ├── objects/
│   └── user/
│
_contrast_profile/                 # Experimento Contrast Profile (ARQUIVADO)
_contrast_profile_ex/              # Variante Contrast (ARQUIVADO)
│
_holdout_2/                        # Holdout set para validação (ARQUIVADO)
```

### Estrutura Interna dos Diretórios `targets`

Cada diretório `_*/` segue a mesma estrutura:

- **meta/**: Metadados do grafo de dependências (qual node executou, quando, hash dos inputs)
- **objects/**: Ficheiros binários RDS com os resultados de cada node do DAG
- **user/**: Configurações específicas do utilizador
- **workspaces/** (opcional): Workspaces R salvos durante execução

**Importante:** Estes diretórios são gerados automaticamente pelo `targets` e não devem ser editados manualmente. Para limpar/reconstruir: `targets::tar_destroy()` ou `targets::tar_prune()`.

---

## Algoritmo Implementado

### Matrix Profile / FLOSS

**FLOSS** (Fast Low-cost Online Semantic Segmentation) é uma adaptação do **FLUSS** para streaming em tempo real.

**Características:**

- ✅ Implementado em R/C++ (via Rcpp)
- ✅ Apropriado para streaming (250 Hz)
- ✅ Processamento em tempo real com janelas deslizantes
- ✅ Baixo consumo de CPU e memória (otimizado para wearables)
- ✅ Não requer treino prévio (unsupervised)

**Hiperparâmetros em Grid Search:**

| Parâmetro | Range | Step | Valores | Descrição |
|-----------|-------|------|---------|-----------|
| `window_size` | 350-400 | 25 | 3 | Tamanho da janela do Matrix Profile |
| `regime_threshold` | 0.05-0.9 | 0.05 | 18 | Limiar para deteção de mudança |
| `regime_landmark` | 2-9 | 0.5 | 15 | Atraso temporal (em segundos) onde o threshold é aplicado no arc curve |

**Total de combinações:** 3 × 18 × 15 = **810 modelos** por registo

**Configurações adicionais (fixas):**

- `var_mp_batch`: 100 (tamanho batch para processamento online)
- `var_mp_history`: 5000 (20s × 250 Hz - buffer histórico em memória)
- `var_mp_threshold`: [0, 0.5] (threshold para cálculo do MP)

---

## Métricas de Avaliação

### Implementadas (Atual)

**`score_pr()` - F-score Adaptado**

- Métrica customizada com penalização por distância temporal
- Parâmetros:
  - `truth`: Ground truth (posições reais das mudanças)
  - `pred`: Predições (posições detetadas)
  - `sample_freq`: 250 Hz
  - `tolerance_long`: 10 segundos
  - `tolerance_short`: 4 segundos
- Cálculo: `1 - score_pr()` (invertido para maximização)

**Baseline Comparativo:**

- `baseline`: Detecções a cada 1 segundo (250 samples)
- `baseline4`: Detecções a cada 4 segundos (1000 samples)

### Planeadas (Compatibilidade com Projeto Python)

Para permitir comparação direta com os detectores do projeto [ts-segmentation](https://github.com/franzbischoff/ts-segmentation):

**F-scores:**

- `f3_weighted`: F3-score com peso temporal
- `f3_classic`: F3-score clássico
- `f1_weighted`: F1-score com peso temporal
- `f1_classic`: F1-score clássico

**NAB (Numenta Anomaly Benchmark):**

- `nab_standard`: Profile standard do NAB
- `nab_low_fp`: Profile que penaliza falsos positivos
- `nab_low_fn`: Profile que penaliza falsos negativos

**Métricas Temporais:**

- `Recall@4s`: Recall com janela de 4 segundos
- `Recall@10s`: Recall com janela de 10 segundos
- `Precision@4s`: Precision com janela de 4 segundos
- `Precision@10s`: Precision com janela de 10 segundos
- `FP/min`: False Positives por minuto
- `EDD Median`: Expected Detection Delay (mediana)

---

## Análises e Relatórios (Workflowr)

O projeto usa o framework `workflowr` para análises reprodutíveis.

```
analysis/
├── _site.yml                      # Configuração do site
├── index.Rmd                      # Página inicial
├── about.Rmd                      # Sobre o projeto
├── license.Rmd                    # Licença
├── report.Rmd                     # Relatório principal
│
├── blog-202108.Rmd                # Blog posts históricos
├── blog-202110.Rmd
├── blog-202201.Rmd
├── blog-202204.Rmd
│
├── regime_optimize.Rmd            # Análises de otimização
├── regime_optimize_2.Rmd
├── regime_optimize_3.Rmd
├── regime_optimize_3_5.Rmd
├── regime_optimize_4.Rmd
├── regime_optimize_4_5.Rmd
│
├── shiny/                         # Apps Shiny interativos
├── shiny_land/
├── shiny_ventricular/
└── shiny_vtachy/
```

**Outputs renderizados:**
```
docs/                              # Site estático gerado (GitHub Pages)
├── index.html
├── about.html
├── figure/                        # Figuras geradas
└── site_libs/                     # Bibliotecas JavaScript/CSS
```

**Preview local:**
```r
# Live preview do workflowr
workflowr::wflow_view()

# Ou via task VS Code
# Task: "R: workflowr: Live-preview workflowr"
```

---

## Tese de Doutoramento

```
thesis/
├── index.Rmd                      # Configuração principal (YAML)
├── 01-chap1.Rmd                   # Capítulo 1: Introdução
├── 02-chap2.Rmd                   # Capítulo 2: Literatura
├── 03-chap3.Rmd                   # Capítulo 3: Metodologia
├── 04-chap4.Rmd                   # Capítulo 4: Resultados
├── 05-chap5.Rmd                   # Capítulo 5: Discussão
├── 06-conclusion.Rmd              # Conclusão
├── 99-references.Rmd              # Referências
├── 00-abstract.Rmd                # Abstract
├── 00-acknowledgements.Rmd        # Agradecimentos
├── 00-dedication.Rmd              # Dedicatória
│
├── template.tex                   # Template LaTeX
├── preamble.tex                   # Preâmbulo LaTeX
├── _bookdown.yml                  # Configuração bookdown
│
└── bib/                           # Bibliografia
    └── references.bib
```

**Compilação:**
```r
# Render completo da tese
bookdown::render_book('thesis')

# Ou via task VS Code
# Task: "R: workflowr: Render thesisdown"
```

---

## Projeto Paralelo - Python Baseline

Para comparação, existe um projeto paralelo em Python que implementa detectores de concept drift:

**Repositório:** [ts-segmentation](https://github.com/franzbischoff/ts-segmentation)

**Detectores implementados:**

- ADWIN (Adaptive Windowing)
- KSWIN (Kolmogorov-Smirnov Windowing)
- HDDM_A (Hoeffding's Drift Detection Method - A variant)
- HDDM_W (Hoeffding's Drift Detection Method - W variant)
- Page-Hinkley

**Framework:** scikit-multiflow

### Comparação: Python vs R

| Aspecto | Python (ts-segmentation) | R (false.alarm) |
|---------|-------------------------|-----------------|
| **Detectores** | ADWIN, KSWIN, HDDM_A/W, Page-Hinkley | FLOSS/Matrix Profile |
| **Paradigma** | Concept drift (supervised) | Semantic segmentation (unsupervised) |
| **Framework** | scikit-multiflow | Implementação custom (Rcpp) |
| **Formato Output** | CSV (predictions_intermediate.csv) | RDS nativo R |
| **Métricas** | F1/F3, NAB, Temporal metrics | F-score adaptado customizado |
| **Organização** | `results/detector_name/` | `output/` (flat) |
| **Visualizações** | PNG plots automáticos | Rmd (workflowr) |
| **Documentação** | README.md por detector | Output único + workflowr |
| **Datasets** | PhysioNet (mesmos) | PhysioNet (mesmos) |
| **Objetivo** | Baseline comparativo | Implementação principal |

**Nota:** DDM e EDDM foram removidos do projeto Python por serem inadequados para séries temporais contínuas (projetados para streams de labels binários).

---

## Desenvolvimento

### Ambiente

```
.devcontainer/                     # Configuração Dev Container (VS Code)
├── devcontainer.json
└── Dockerfile

renv/                              # Gestão de dependências R
├── library/                       # Bibliotecas instaladas
├── settings.json
└── activate.R

.vscode/                           # Configurações VS Code
├── tasks.json                     # Tasks automatizadas
└── settings.json
```

### Testes

```
tests/
├── testthat/                      # Testes unitários
│   ├── test-*.R
│   └── helper-*.R
└── testthat.R
```

**Executar testes:**
```r
# Via devtools
devtools::test()

# Ou via task
# Task: "R: routine: Test units"
```

### Code Quality

**Linting:**
```r
# Task: "R: routine: Lint project"
# Verifica: analysis/, R/, scripts/, review/, thesis/, output/
```

**Styling:**
```r
# Task: "R: routine: Style project"
# Aplica styler em todo o projeto
```

**Coverage:**
```r
# Task: "R: misc: test coverage"
covr::package_coverage()
```

---

## Estado Atual do Projeto

### Fase 1: Deteção de Mudanças de Regime

**Status:** 🔄 Em desenvolvimento ativo

#### ✅ Completo

- [x] Implementação Matrix Profile (MASS, STOMP, SCRIMP, STAMP)
- [x] Implementação FLOSS para deteção de regimes
- [x] Leitura e pré-processamento de datasets ECG
- [x] Pipeline de grid search para hiperparâmetros
- [x] Cálculo de scores com métrica customizada
- [x] Baseline comparativo (1s e 4s)
- [x] Processamento dataset `afib_regimes` (paroxysmal_afib)
- [x] Processamento dataset `vtachyarrhythmias`

#### 🔄 Em Desenvolvimento

- [ ] Script de conversão R → CSV (formato Python)
- [ ] Cross-validation 5-folds completo
- [ ] Implementação métricas NAB
- [ ] Implementação métricas temporais (Recall@Xs, Precision@Xs, FP/min, EDD)
- [ ] Compatibilidade de formato com projeto Python
- [ ] Estrutura de diretórios `results/floss/` similar ao Python

#### ⏳ Planeado

- [ ] Processamento dataset `malignantventricular`
- [ ] Processamento dataset `arrhythmias` (requer adaptação)
- [ ] Análise comparativa FLOSS vs detectores Python
- [ ] Otimização fine-tuning de hiperparâmetros
- [ ] Visualizações automáticas (similar ao Python)
- [ ] Documentação por experimento

### Fase 2: Classificação de Mudanças

**Status:** ⏸️ Pausado (aguardando conclusão Fase 1)

**Objetivo:** Classificar mudanças detetadas em:

- Alterações supérfluas (falsos positivos)
- Alterações graves:
  - Asystole
  - Extreme Bradycardia
  - Extreme Tachycardia
  - Ventricular Tachycardia
  - Ventricular Flutter/Fibrillation

**Abordagem planejada:** Machine Learning (BART, Random Forest, etc.)

---

## Workflows e Tasks

O projeto inclui múltiplas tasks automatizadas (`.vscode/tasks.json`):

### Pipeline Principal

- `R: targets: Run main pipeline` - Executa pipeline targets principal
- `R: targets: Run regime pipeline` - Pipeline deteção regimes
- `R: targets: Run classifier pipeline` - Pipeline classificação (Fase 2)

### Desenvolvimento

- `R: routine: Style project` - Formata código
- `R: routine: Lint project` - Verifica code quality
- `R: routine: Test units` - Executa testes
- `R: routine: Update docs and self install` - Atualiza documentação

### Workflowr

- `R: workflowr: Pre-build workflowr` - Pré-compila relatórios
- `R: workflowr: Live-preview workflowr` - Preview local (porta 4000)
- `R: workflowr: Render thesisdown` - Compila tese
- `R: workflowr: Live-preview thesisdown` - Preview tese

### Manutenção

- `R: routine: Update renv and isolate` - Atualiza dependências
- `R: targets: zDESTROY targets` - Remove cache targets
- `R: targets: zPRUNE targets` - Limpa objetos não usados

---

## Referências Principais

**Matrix Profile:**

- Yeh et al. (2016). "Matrix Profile I: All Pairs Similarity Joins for Time Series"
- Zhu et al. (2016). "Matrix Profile II: Exploiting a Novel Algorithm and GPUs"
- Gharghabi et al. (2017). "Matrix Profile VIII: Domain Agnostic Online Semantic Segmentation at Superhuman Performance Levels"

**FLUSS/FLOSS:**

- Gharghabi et al. (2017). "Domain Agnostic Online Semantic Segmentation for Multi-Dimensional Time Series"

**Datasets:**

- Moody & Mark (2001). "MIT-BIH Arrhythmia Database"
- Greenwald (1986). "MIT-BIH Malignant Ventricular Ectopy Database"
- Nolle et al. (1986). "CAMUS: A Context Ambulatory Monitoring System"
- Petrutiu et al. (2007). "Abrupt Changes in Fibrillatory Wave Characteristics"

**Challenge:**

- Clifford et al. (2015). "PhysioNet/CinC Challenge 2015: Reducing False Arrhythmia Alarms in the ICU"

---

## Licença

MIT License - Ver `LICENSE.md`

---

## Contacto

**Autor:** Francisco Bischoff \
**Repositório:** https://github.com/franzbischoff/false.alarm \
**Projeto Python:** https://github.com/franzbischoff/ts-segmentation

---

**Legenda de Status:**

- ✅ Completo e testado
- 🔄 Em desenvolvimento ativo
- ⏳ Disponível mas não implementado
- ⏸️ Pausado (aguardando outra fase)
- 🎯 Foco atual
- ⚠️ Depreciado
- 📚 Arquivado para referência
