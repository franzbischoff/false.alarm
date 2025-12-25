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

## 2025-12-21 — Debug check_interactions parallel issue

- Context: Working in `scripts/regime_detection/evaluation/parameter_analysis.R`, reached the `check_interactions()` step.
- Issue: Interaction analysis not working when `parallel = TRUE`; debugging with non-parallel runs and smaller datasets.
- Recent change: `best_fit` cannot be cached because leads to invalid results.

(Atualizado em 2025-12-21)

---

## 2025-12-25 — Análise de Importância de Parâmetros: Interpretação de Interações

### Script de Análise
`scripts/regime_detection/evaluation/parameter_analysis.R` implementa análise de importância dos hiperparâmetros FLOSS usando modelo BART com 3 métodos:
- **FIRM** (Feature Importance Ranking Measure via ICE curves)
- **Permutation Importance**
- **SHAP** (SHapley Additive exPlanations)

### **CRÍTICO: Interpretação dos Valores de Interação**

#### Regra de Interpretação
**Sempre usar o range OBSERVADO da métrica, não o teórico:**

```r
# CORRETO: Usar range observado nos dados
observed_range <- max(metric_values) - min(metric_values)
interaction_percentage <- (interaction_value / observed_range) * 100

# INCORRETO: Usar range teórico [0, 1]
# Subestima a verdadeira magnitude da interação
```

#### Exemplo Real (malignantventricular, f3_weighted)
- **Range teórico**: [0, 1.0]
- **Range observado**: [0, 0.744] ← usar este!
- **Interação window_size × regime_threshold**: 0.0748
  - Interpretação correta: 0.0748 / 0.744 = **10.05%** do desempenho alcançável
  - Interpretação errada: 0.0748 / 1.0 = 7.48% (subestima)

#### Justificação Técnica
1. O modelo BART foi treinado no range [0, 0.744], não no teórico
2. Nenhuma das 25,920 configurações alcançou valores fora deste range
3. A interação mede desvio do efeito aditivo **dentro do espaço observado**
4. Comparações entre datasets devem usar os respetivos ranges observados

#### Rankings de Interação (Exemplo)
```
window_size × regime_threshold:     10.0% → FORTE (tunagem conjunta obrigatória)
regime_threshold × regime_landmark:  8.4% → FORTE
regime_threshold × min_gap_samples:  0.8% → FRACA
window_size × min_gap_samples:       0.1% → MUITO FRACA (quase independentes)
```

**Para o relatório**: Sempre reportar interações como percentagem do range observado com interpretação clara de independência vs. acoplamento de parâmetros.

(Atualizado em 2025-12-25)
