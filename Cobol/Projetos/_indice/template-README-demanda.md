# AGROLEG-XXXX - [TITULO RESUMIDO]

## Status workflow
- Fase atual: (Inicializacao / Analise em andamento / Analise concluida / Plano proposto / Implementacao / Certificacao de riscos / Pre-subida / Homologacao / Encerrada)
- Ultimo comando:
- Proximo passo:

## Objetivo
Descrever objetivo funcional e tecnico da demanda (negocio + escopo).

## Requisito de negocio
- Iniciativa: (ex.: SICOR 5.12 — ver agroleg-knowledge/initiatives/SICOR-5.12.md / N/A)
- Mensagens COR/CIR desta historia: (ex.: COR0005 / N/A)
- Regra de negocio resumida:

## Centros de custo impactados
- (ex.: RUEC, RUCA, RCOR)

## Escopo tecnico
- COBOL:
- JCL:
- CICS:
- DB2:
- IMS:
- BATCH:

## Malha batch
- Fonte JSON: `Projetos/_indice/referencia-batch-malha/malhas/AGROLEG-XXXX-<fluxo>.json`
- Visual HTML: `Projetos/_indice/referencia-batch-malha/output/AGROLEG-XXXX-<fluxo>.html`
- Gerar: `powershell -ExecutionPolicy Bypass -File "Projetos/_indice/referencia-batch-malha/scripts/gerar_malha_html.ps1" -MalhaPath "<json>"`

## Fontes impactados (preliminar)
- Programa(s):
- Copybook(s):
- DCLGEN(s):
- JCL(s):
- Mapa(s) BMS:

## Estrutura de pastas (componentes)

Pastas por tipo na raiz da demanda: `pgm/`, `cpy/`, `jcl/`, `net/`, `mfs/`, `db2/` (conforme escopo).

- **Escopo (alteracao):** `<componente>/` (ex.: `cpy/i#cbon01.cpy`)
- **Somente analise:** `<componente>/analise/` (ex.: `cpy/analise/i#cbon01-prd.cpy`)
- **Malha batch:** `net/` (exports de rede de jobs)

Ver `Projetos/_indice/guia-organizacao-seguranca.md`.

## Fontes pendentes baixa
| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | PGM | | escopo | pgm/ | pendente / baixado |
| P2 | CPY | | comparar PRD | cpy/analise/ | pendente / baixado |
| P1 | NET | | malha batch | net/ | pendente / baixado |

## Impacto tecnico (confirmado)
Preencher quando analise estiver concluida (Fase 2) e refinar apos implementacao (Fase 4).

- Programas alterados:
- Copybooks alterados:
- Tabelas DB2 (via DCLGEN):
- Fluxo resumido:

## Plano de ajuste (proposta)
Preencher na Fase 3. Implementar somente apos validacao explicita do usuario.

| # | Fonte | Area/Section | Alteracao proposta | Referencia |
|---|---|---|---|---|
| 1 | | | | cap. 03 + cap. 01 (exemplo) |

### Ordem de implementacao
1.

### Riscos do plano
-

### Cenarios de teste (preliminar)
-

## Decisoes tecnicas
- (ex.: exclusao SICOR com CG2605* e verbos inviaveis — prefixo Capgemini CGaamm)

## Historias relacionadas
| Historia | Relacao | Observacao |
|---|---|---|
| AGROLEG-XXXX | pre-requisito / complementa / mesma-mensagem / mesmo-programa / mesma-tabela / mesmo-modulo | |

Relacoes validas: `pre-requisito`, `complementa`, `mesma-mensagem`, `mesmo-programa`, `mesma-tabela`, `mesmo-modulo`, `substitui`, `conflita`.

## Licoes para proximas analises
- (pontos que aceleram impacto em demandas futuras)

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - Nenhuma
- DCLGEN (encontradas):
  - Nenhuma
- Pendencias (nao encontradas em LIBS):
  - Nenhuma
- Dependencias de sistema:
  - Nenhuma

## Riscos e atencoes
-

## Certificacao de riscos (pos-desenvolvimento)

Preencher na **Fase 4b** (`Certificar riscos <ID>`). Somente riscos de codificacao.

| # | Risco (origem) | Veredito | Evidencia |
|---|----------------|----------|-----------|
| | | | |

Vereditos: `NAO REALIZADO` | `MITIGADO PARCIAL` | `REALIZADO` | `N/A` | `PENDENTE TESTE` (nao codificacao — Fase 6)

## Plano de testes
- [ ] Cenario funcional principal
- [ ] Regressao relacionada
- [ ] Validacao SICOR 5.12 (se aplicavel — checklist em initiatives/SICOR-5.12.md)

## Sequencia projeto (Capgemini — subida MF)

Preencher na **Fase 5b — Subida MF** quando consultoria = Capgemini. Informar o codigo em pacotes Changeman, tickets de promocao e implantacao PRD.

- Codigo: **2021-0765286-5-035** (contrato alocacao AGROLEG)
- Stefanini / 7Comm: usar sequencia propria da consultoria (ex.: `STEF-001/Anotações`)

## Pacotes changeman

Preencher na **Fase 5b — Subida MF** (`demanda-workflow`).

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| | Subida (implementacao) | | pendente | `pgm/`, `cpy/` (escopo alterado) |
| | Contingencia (versao PRD pre-demanda) | | pendente | `pgm/analise/`, `cpy/analise/` (baseline) |

- **Subida:** pacote Changeman com fontes alterados da demanda; registrar numero do pacote e status do compile.
- **Contingencia:** pacote Changeman com versao producao (baseline) para retorno em caso de problema pos-implantacao; registrar numero do pacote.

## Promocao ambientes (Jira)

Preencher na **Fase 5b — Subida MF** (`demanda-workflow`).

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| | | | | pendente |

- Ticket Jira de **promocao** dos pacotes Changeman entre ambientes (TST / HOM / PRD).

## Evidencias
- (logs, prints, pacotes changeman, ticket Jira promocao)

## Dados de origem
- Consultoria: (Capgemini / Stefanini / 7Comm)
- Canal: (Jira XML / Reuniao — ver intake.md)
- Referencia externa: (chave Jira, ticket, mudanca) ou N/A
- Chave Jira (Capgemini): AGROLEG-XXXX
- Titulo:
- Status:
- Responsavel:
- Atualizado em:

## Status
- [ ] Em andamento
- [ ] Em teste (dev interno — antes da subida MF)
- [ ] Pronto para subida (pre-subida aprovada)
- [ ] Em homologacao (gestor testa **apos** subida MF)
- [ ] Implantado (PRD — apos OK do gestor)
