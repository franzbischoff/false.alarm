## Contexto principal do Projeto

Este repositório contém todo o código e documentação de uma tese de doutoramento focada na deteção de mudanças de regime (concept drift / change points) em sinais de ECG em fluxo (250 Hz). A primeira parte deste doutoramento foca-se na detecção das mudanças de regime, enquanto que a segunda parte se concentra na classificação das alterações detetadas para diferenciar entre alterações supérfluas (ou falsos positivos) e alterações relevantes associadas a cinco tipos de alterações graves do ritmo cardíaco (Asystole, Extreme Bradycardia, Extreme Tachycardia, Ventricular Tachycardia and Ventricular Flutter/Fibrillation). A ideia inicial vem do CinC/Physionet Challenge 2015, que visava reduzir os alarmes falsos. Entretanto esta tese de doutoramento evoluiu para um foco mais amplo na deteção de mudanças de regime em sinais de ECG com a nuance de focar na implementação destes modelos em cenários de "low CPU and low memory", como dispositivos wearables. A pergunta principal é "can we accomplish this objective using a minimalist approach (low CPU, low memory) while maintaining robustness?"

Inicialmente este projeto iniciou na linguagem R, utilizando dois frameworks para reprodutibilidade, o package `workflowr` que ajuda na organização dos relatórios e o package `targets` que ajuda na organização do pipeline de processamento. No entanto a implementação inicial baseou-se na premissa de que o algoritmo de detecção de mudanças era um "modelo" que seria treinado com hiperparâmetros, mas na verdade trata-se de um algoritmo que necessita apenas de um grid search para encontrar os melhores parâmetros. Assim, neste momento eu reiniciei a pipeline diretamente, sem o `targets` usando os ficheiros no formato regex `\.\/scripts\/(\d+)_([a-zA-Z_]{2,})\.R`.

O algoritmo que quero implementar baseia-se no Matrix Profile (MP) desenvolvido pela Universidade California Riverside (UCR). O MP é calculado em realtime, ótimo para streaming. A grande vantagem de calcular o MP é que assim que está calculado, permite aplicar diversos algoritmos em cima do MP, dentre eles deteção de motifs, discords e também deteção de mudanças de regime. A ideia é calcular o MP em janelas deslizantes (sliding windows) e aplicar os algoritmos de deteção de mudanças de regime em cima do MP. O algoritmo de deteção de mudanças de regime que pretendo implementar é o FLOSS (Fast Low-cost Online Semantic Segmentation) que é uma versão modificada do FLUSS (Fast Low-cost Unipotent Semantic Segmentation) apropriada para streaming.

Caso seja necessário informações sobre a estrutura do projeto em R, consulte o ficheiro `PROJECT_STRUCTURE.md`.

## Projeto paralelo em Python

Paralelamente, de forma a criar um baseline para comparação com o algoritmo de MP/FLOSS, foi implementado um baseline em Python que inclui diversos detectores de mudanças de regime apropriados para séries temporais que utiliza o scikit-multiflow e implementa os algoritmos "adwin", "kswin", "hddm_a", "hddm_w" e "page_hinkley". Este baseline analisa os mesmos datasets que este projeto em R, permitindo uma comparação direta entre os detectores implementados em Python e o algoritmo de MP/FLOSS implementado em R. As métricas de avaliação incluem "f3_weighted", "f3_classic", "f1_weighted", "f1_classic", "nab_standard", "nab_low_fp" e "nab_low_fn", bem como métricas temporais como "Recall@4s", "Recall@10s", "Precision@4s", "Precision@10s", "FP/min" e "EDD Median".

O projeto está hospedado em: https://github.com/franzbischoff/ts-segmentation

**Nota Importante**: DDM e EDDM foram removidos do projeto por serem inadequados para análise de séries temporais. Estes detectores foram projetados para concept drift em classificação binária (streams de labels), não para detecção de mudanças em valores contínuos.

## Instruções Importantes
Não crie ficheiros de documentação Markdown adicionais sem antes perguntar ao utilizador. Todas as alterações de documentação devem ser feitas nos ficheiros existentes, a menos que o utilizador solicite explicitamente a criação de novos ficheiros.

## Memória Persistente
Sempre que o utilizador indicar que está a iniciar os trabalhos do dia, consulte o ficheiro `.github/copilot-memory.md` para obter as informações mais recentes sobre o estado do projeto.
Quando o utilizador informar que encerrou os trabalhos do dia, atualize o ficheiro `.github/copilot-memory.md` com as informações mais recentes sobre o estado do projeto.

## Tarefas Atuais

Visto que o projeto em Python já está a fazer a avaliação dos resultados dos detectores, neste projeto em R iremos neste momento focar-nos em transformar o dataset já processado pelo novo script simplificado da pipeline (sem o `targets`) para o formato esperado pelo script de avaliação em Python. Isto implica criar um novo script em R que leia os ficheiros de resultados gerados pelo algoritmo de MP/FLOSS e os converta para o formato CSV esperado pelo script de avaliação em Python, garantindo que todas as métricas necessárias estão corretamente calculadas e formatadas.

## Hiperparâmetros do FLOSS - Grid Search Pendente

Atualmente o algoritmo FLOSS está a utilizar os seguintes hiperparâmetros:

1. **window_size**: 350-400 (step 25) → 3 valores
2. **regime_threshold**: 0.05-0.9 (step 0.05) → 18 valores  
3. **regime_landmark**: 2-9 (step 0.5) → 15 valores (atraso temporal em segundos onde o threshold é aplicado)

**IMPORTANTE - Grid Search Adicional Necessário:**

4. **min_gap_samples** (atualmente fixo em 200): Parâmetro da função `clean_pred(200, FALSE)` que define a distância mínima (em samples) entre predições consecutivas. No projeto Python, este parâmetro foi testado com os valores: **1000, 2000, 3000, 5000** samples.

   - Valor atual: 200 samples (0.8s a 250Hz)
   - Valores a testar: 1000, 2000, 3000, 5000 samples (4s, 8s, 12s, 20s a 250Hz)
   - Localização no código: `scripts/1_compute_all_scores.R`, linha onde `clean_pred(200, TRUE)` é chamado
   - **Ação pendente**: Adicionar `min_gap_samples` ao grid search completo para comparabilidade com os detectores Python

## Tarefas Futuras

Lembre-se que este doutoramento consistem em duas fases. Estamos a trabalhar na primeira fase. Após a conclusão da primeira fase, que envolve a detecção de mudanças de regime, o próximo passo será focar na segunda fase do doutoramento, que é a classificação das mudanças de regime detetadas. Esta fase envolverá o desenvolvimento e implementação de modelos de machine learning para classificar as mudanças de regime em categorias relevantes, diferenciando entre alterações supérfluas e alterações graves do ritmo cardíaco.
