# Copilot Memory

## 2025-12-20 — Fase 1 Completa: Pipeline Modular de Regime Detection

### Estado Atual do Projeto

**✅ FASE 1 CONCLUÍDA**: A deteção de mudanças de regime (primeira fase do doutoramento) está completa do ponto de vista computacional.

### Pipeline Implementada

Pipeline modular em `scripts/regime_detection/` **sem framework `targets`**:

1. **Script 10** (`10_prepare_data.R`): Preparação e normalização dos dados ECG
   - Input: Ficheiros ECG raw dos datasets
   - Output: `output/regime_detection/{dataset}/generation/tidy_dataset.rds`

2. **Script 20** (`20_generate_matrix_profiles.R`): Computação de Matrix Profiles
   - Usa `matrixprofiler` package via `scripts/helpers/compute_mp_floss.R`
   - **Não usa código C++ do diretório `src/`** (implementação antiga)
   - Output: `matrix_profiles_w{size}.rds` para cada window_size (25-400, 16 ficheiros)

3. **Script 30** (`30_predict_grid_search.R`): Grid search exaustivo
   - Usa `matrixprofiler` package via `scripts/helpers/predict_floss_changes.R`
   - Grid: 16 window_size × 18 threshold × 15 landmark × 6 min_gap = **25,920 combinações/registo**
   - Output: `output/regime_detection/{dataset}/prediction/predictions_grid.rds`

4. **Script 40** (`40_convert_to_csv.R`): Conversão para formato Python
   - Output: `output/regime_detection/{dataset}/{dataset}_predictions.csv`

### Hiperparâmetros do Grid Search

- **window_size**: 25-400 (step 25) → 16 valores
- **regime_threshold**: 0.05-0.9 (step 0.05) → 18 valores
- **regime_landmark**: 2-9 (step 0.5) → 15 valores
- **min_gap_samples**: 200, 500, 1000, 2000, 3000, 5000 → 6 valores

### Datasets Processados

✅ Computação completa para 3 datasets:
- `afib_regimes`
- `malignantventricular`
- `vtachyarrhythmias`

Todos com resultados exportados em CSV prontos para avaliação no projeto Python.

### Próximos Passos Potenciais

A Fase 1 está completa. Aguardam-se instruções para:
- Análise/visualização dos resultados em R
- Integração com projeto Python para avaliação comparativa
- Preparação para Fase 2 (classificação de mudanças de regime)

---

(Atualizado em 2025-12-20)
