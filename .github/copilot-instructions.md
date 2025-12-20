## Contexto principal do Projeto

Este repositório contém todo o código e documentação de uma tese de doutoramento focada na deteção de mudanças de regime (concept drift / change points) em sinais de ECG em fluxo (250 Hz). A primeira parte deste doutoramento foca-se na detecção das mudanças de regime, enquanto que a segunda parte se concentra na classificação das alterações detetadas para diferenciar entre alterações supérfluas (ou falsos positivos) e alterações relevantes associadas a cinco tipos de alterações graves do ritmo cardíaco (Asystole, Extreme Bradycardia, Extreme Tachycardia, Ventricular Tachycardia and Ventricular Flutter/Fibrillation). A ideia inicial vem do CinC/Physionet Challenge 2015, que visava reduzir os alarmes falsos. Entretanto esta tese de doutoramento evoluiu para um foco mais amplo na deteção de mudanças de regime em sinais de ECG com a nuance de focar na implementação destes modelos em cenários de "low CPU and low memory", como dispositivos wearables. A pergunta principal é "can we accomplish this objective using a minimalist approach (low CPU, low memory) while maintaining robustness?"

O algoritmo implementado baseia-se no Matrix Profile (MP) desenvolvido pela Universidade California Riverside (UCR). O MP é calculado em realtime, ótimo para streaming. A grande vantagem de calcular o MP é que assim que está calculado, permite aplicar diversos algoritmos em cima do MP, dentre eles deteção de motifs, discords e também deteção de mudanças de regime. A ideia é calcular o MP em janelas deslizantes (sliding windows) e aplicar os algoritmos de deteção de mudanças de regime em cima do MP. O algoritmo de deteção de mudanças de regime implementado é o FLOSS (Fast Low-cost Online Semantic Segmentation) que é uma versão modificada do FLUSS (Fast Low-cost Unipotent Semantic Segmentation) apropriada para streaming.

### Arquitetura da Pipeline de Deteção de Regime

A pipeline de deteção de mudanças de regime está implementada em **scripts modulares** localizados em `scripts/regime_detection/` e **não utiliza mais o framework `targets`**. A pipeline foi simplificada em 4 scripts sequenciais (10, 20, 30, 40) organizados em 2 fases principais:

#### FASE 1: GENERATION (Computação de Matrix Profiles)
- **Script 10** (`10_prepare_data.R`): Preparação e normalização dos dados ECG
- **Script 20** (`20_generate_matrix_profiles.R`): Computação dos Matrix Profiles usando o package `matrixprofiler` (não usa código C++ do diretório `src/`)

#### FASE 2: PREDICTION (Grid Search e Exportação)
- **Script 30** (`30_predict_grid_search.R`): Grid search exaustivo sobre hiperparâmetros usando funções do package `matrixprofiler` via helpers em `scripts/helpers/predict_floss_changes.R`
- **Script 40** (`40_convert_to_csv.R`): Conversão dos resultados para formato CSV compatível com avaliação em Python

**IMPORTANTE**: O código C++ no diretório `src/` foi utilizado em implementações anteriores com `targets`, mas a pipeline atual utiliza exclusivamente o package `matrixprofiler` através dos helpers:
- `scripts/helpers/compute_mp_floss.R` (usado pelo script 20)
- `scripts/helpers/predict_floss_changes.R` (usado pelo script 30)

Os resultados são salvos em `output/regime_detection/{dataset}/` com estrutura organizada por dataset (afib_regimes, malignantventricular, vtachyarrhythmias).

Caso seja necessário informações sobre a estrutura histórica do projeto em R (implementações antigas com `targets`), consulte o ficheiro `PROJECT_STRUCTURE.md`.

## Projeto paralelo em Python

Paralelamente, de forma a criar um baseline para comparação com o algoritmo de MP/FLOSS, foi implementado um baseline em Python que inclui diversos detectores de mudanças de regime apropriados para séries temporais que utiliza o scikit-multiflow e implementa os algoritmos "adwin", "kswin", "hddm_a", "hddm_w" e "page_hinkley". Este baseline analisa os mesmos datasets que este projeto em R, permitindo uma comparação direta entre os detectores implementados em Python e o algoritmo de MP/FLOSS implementado em R. As métricas de avaliação incluem "f3_weighted", "f3_classic", "f1_weighted", "f1_classic", "nab_standard", "nab_low_fp" e "nab_low_fn", bem como métricas temporais como "Recall@4s", "Recall@10s", "Precision@4s", "Precision@10s", "FP/min" e "EDD Median".

O projeto está hospedado em: https://github.com/franzbischoff/ts-segmentation

**Nota Importante**: DDM e EDDM foram removidos do projeto por serem inadequados para análise de séries temporais. Estes detectores foram projetados para concept drift em classificação binária (streams de labels), não para detecção de mudanças em valores contínuos.

## Instruções Importantes
Não crie ficheiros de documentação Markdown adicionais sem antes perguntar ao utilizador. Todas as alterações de documentação devem ser feitas nos ficheiros existentes, a menos que o utilizador solicite explicitamente a criação de novos ficheiros.

## Memória Persistente
Sempre que o utilizador indicar que está a iniciar os trabalhos do dia, consulte o ficheiro `.github/copilot-memory.md` para obter as informações mais recentes sobre o estado do projeto.
Quando o utilizador informar que encerrou os trabalhos do dia, atualize o ficheiro `.github/copilot-memory.md` com as informações mais recentes sobre o estado do projeto.

## Status Atual da Fase 1 (Deteção de Mudanças de Regime)

✅ **CONCLUÍDO**: A primeira fase do doutoramento (deteção de mudanças de regime) está completa do ponto de vista de computação.

- ✅ Pipeline modular implementada (scripts 10, 20, 30, 40) sem dependência do framework `targets`
- ✅ Matrix Profiles computados para todos os window sizes usando `matrixprofiler` package
- ✅ Grid search exaustivo executado sobre todas as combinações de hiperparâmetros
- ✅ Resultados exportados para formato CSV compatível com o sistema de avaliação em Python
- ✅ Dados prontos para comparação com baselines (adwin, kswin, hddm_a, hddm_w, page_hinkley)

Os resultados do FLOSS estão salvos em `output/regime_detection/{dataset}/{dataset}_predictions.csv` e podem ser avaliados no projeto Python (https://github.com/franzbischoff/ts-segmentation) usando as mesmas métricas dos outros detectores.

## Hiperparâmetros do FLOSS - Grid Search Completo

O algoritmo FLOSS foi testado com grid search exaustivo sobre 4 dimensões de hiperparâmetros (implementado em `scripts/regime_detection/30_predict_grid_search.R`):

1. **window_size**: 25-400 (step 25) → **16 valores**
   - Representa o tamanho da janela do Matrix Profile (0.1s a 1.6s a 250Hz)
   - Computado na Fase 1 pelo script `20_generate_matrix_profiles.R`

2. **regime_threshold**: 0.05-0.9 (step 0.05) → **18 valores**
   - Sensibilidade para detetar mudanças de regime (quanto maior, mais sensível)
   - Aplicado sobre o Corrected Arc Curve (CAC) do FLOSS

3. **regime_landmark**: 2-9 (step 0.5) → **15 valores**
   - Atraso temporal em segundos onde o threshold é aplicado
   - Representa o delay entre avaliação e detecção no streaming

4. **min_gap_samples**: 200, 500, 1000, 2000, 3000, 5000 → **6 valores**
   - Distância mínima (em samples) entre predições consecutivas
   - Parâmetro da função `clean_pred()` para remover duplicatas
   - Valores em segundos a 250Hz: 0.8s, 2s, 4s, 8s, 12s, 20s
   - Garante comparabilidade com os detectores baseline do projeto Python

**Total de combinações por registo**: 16 × 18 × 15 × 6 = **25,920 configurações**

As predições são geradas em formato RAW e depois aplicadas todas as variações de `min_gap_samples`, permitindo testar diferentes estratégias de supressão de alarmes duplicados.

## Tarefas Futuras

Lembre-se que este doutoramento consistem em duas fases. Estamos a trabalhar na primeira fase. Após a conclusão da primeira fase, que envolve a detecção de mudanças de regime, o próximo passo será focar na segunda fase do doutoramento, que é a classificação das mudanças de regime detetadas. Esta fase envolverá o desenvolvimento e implementação de modelos de machine learning para classificar as mudanças de regime em categorias relevantes, diferenciando entre alterações supérfluas e alterações graves do ritmo cardíaco.
