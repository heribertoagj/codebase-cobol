---
name: readme-demanda-template
description: Cria ou inicializa README de demanda AGROLEG a partir do template padrao do projeto. Use ao abrir nova historia, Implementar AGROLEG-nnn.xml ou quando README estiver ausente ou incompleto.
---

# README Demanda Template

## Objetivo

Padronizar a criacao do README de cada demanda AGROLEG, alinhado ao fluxo de 7 fases do `demanda-workflow`.

## Template oficial

`Cobol/Projetos/_indice/template-README-demanda.md`

Guia do usuario: `Cobol/Projetos/_indice/guia-fluxo-agroleg.md`

## Quando usar

- Nova pasta `AGROLEG-nnn` sem README.
- Fase 1 do `demanda-workflow` (`Implementar AGROLEG-nnn.xml`).
- Usuario pede "criar README da demanda".

## Execucao

1. Copiar template para `Cobol/Projetos/<PROJETO>/AGROLEG-nnn/README.md`.
2. Substituir `AGROLEG-XXXX` pela chave real.
3. Preencher titulo resumido no cabecalho.
4. Manter secoes vazias com placeholder — nao remover estrutura.
5. Sync Jira se existir XML.
6. Dependencias LIBS com `-InPlace` quando houver fontes em `pgm/`, `cpy/`, etc.

## Secoes obrigatorias (nao remover)

| Secao | Fase workflow |
|---|---|
| Status workflow | Todas |
| Objetivo | 1 |
| Requisito de negocio | 1 |
| Centros de custo impactados | 1 |
| Escopo tecnico | 1 |
| Fontes impactados (preliminar) | 1–2 |
| Fontes pendentes baixa | 1–2 |
| Impacto tecnico (confirmado) | 2 (analise) + 4 (implementacao) |
| Plano de ajuste (proposta) | 3 |
| Decisoes tecnicas | 4 |
| Historias relacionadas | 1 + 7 |
| Licoes para proximas analises | 7 |
| Malha batch | 2 (se BATCH) |
| Dependencias LIBS | 1–2 (script) |
| Plano de testes | 3–4 |
| Pacotes changeman | 5b (subida MF) |
| Promocao ambientes (Jira) | 5b (subida MF) |
| Evidencias | 5b–7 |
| Dados Jira (sync) | 1 (script) |
| Status | Todas |

## Regras

- Nao inventar fontes antes de validar no codigo ou acervo.
- `Plano de ajuste` e preenchido na Fase 3 — implementacao so apos validacao do usuario.
- `Fontes pendentes baixa` deve listar member, tipo, motivo e prioridade P1/P2/P3.
- `Pacotes changeman` permanece pendente ate a **Fase 5b (Subida MF)** — o agente deve **solicitar** ao usuario: numero do pacote de **subida**, numero do pacote de **contingencia**, members, status do compile e **ticket Jira de promocao** (link) antes de avancar para homologacao.

## Secao Pacotes changeman (estrutura minima)

Incluir no README desde a criacao (placeholder). Preencher na subida:

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| RUCA000xxx | Subida (implementacao) | PGM, CPY... | Compilado / pendente | escopo alterado |
| RUCA000yyy | Contingencia (PRD pre-demanda) | PGM, CPY... | Pacote criado / pendente | baseline em `*/analise/` |

## Secao Promocao ambientes (Jira)

Incluir no README desde a criacao (placeholder). Preencher na subida:

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| TICKET-xxxxx | URL Jira browse | RUCA000xxx | RUCA000yyy | Criado / Em promocao / Implantado |

## Integracao

- Orquestrador: `demanda-workflow`
- Conhecimento: `agroleg-knowledge`
- Malha batch: `malha-batch-analise`
- Dependencias: `readme-dependencias-libs`
- Jira: `jira-xml-readme-sync`
