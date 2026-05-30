---
name: demanda-workflow
description: Orquestra o ciclo ponta a ponta de demandas COBOL — Capgemini (AGROLEG via XML Jira), Stefanini (STEF via reuniao) e 7Comm (7COMM via reuniao). Use para Implementar AGROLEG-nnn.xml, Iniciar demanda Stefanini/7Comm, continuar analise, implementar plano, pre-subida ou finalizar.
---

# Demanda Workflow

## Canais de entrada

| Consultoria | ID | Fase 0 / 1 | Skill |
|---|---|---|---|
| Capgemini | `AGROLEG-nnn` | `Implementar AGROLEG-nnn.xml` | `jira-xml-readme-sync` |
| Stefanini | `STEF-nnn` | `Iniciar demanda Stefanini` | `demanda-intake-reuniao` |
| 7Comm | `7COMM-nnn` | `Iniciar demanda 7Comm` | `demanda-intake-reuniao` |

Fases 2–7 usam o **ID da demanda** nos comandos (substituir `AGROLEG-nnn` por `STEF-nnn` ou `7COMM-nnn`).

---

# Demanda Workflow (detalhe)

## Objetivo

Orquestrar o ciclo completo de uma demanda, com **gates humanos** entre analise, plano, implementacao e encerramento.

## Regra de ouro

| Comando do usuario | Pode alterar fontes (.cbl/.cpy/.mfs/.jcl)? |
|---|---|
| `Iniciar demanda Stefanini` / `Iniciar demanda 7Comm` | **NAO** |
| `Implementar AGROLEG-nnn.xml` | **NAO** |
| `Fontes baixados — continuar analise <ID>` | **NAO** |
| `Gerar plano de ajuste <ID>` | **NAO** |
| `Implementar plano <ID>` | **SIM** (apos validacao explicita) |
| `Pre-subida <ID>` | Correcoes bloqueantes apenas |
| `Finalizar <ID>` | README/acervo apenas |

**Nunca** implementar codigo na Fase 1 ou 2. **Nunca** encerrar acervo antes da homologacao implantada.

---

## Mapa de fases

| Fase | Nome | Gate |
|---|---|---|
| 1 | Inicializacao + impacto preliminar | Usuario baixa fontes |
| 2 | Analise iterativa de fontes | Analise concluida |
| 3 | Plano de ajuste | Usuario valida plano |
| 4 | Desenvolvimento | Pre-subida aprovada |
| 5 | Pre-subida | Usuario sobe MF |
| 6 | Homologacao | Usuario implanta |
| 7 | Encerramento | — |

---

## Comandos do usuario

| Momento | Comando |
|---|---|
| Inicio Stefanini/7Comm (reuniao) | `Iniciar demanda Stefanini` / `Iniciar demanda 7Comm` |
| Inicio Capgemini (XML Jira) | `Implementar AGROLEG-nnn.xml` |
| Apos baixar fontes | `Fontes baixados — continuar analise <ID>` |
| Solicitar plano | `Gerar plano de ajuste <ID>` |
| Apos validar plano | `Implementar plano <ID>` |
| Apos desenvolvimento | `Pre-subida <ID>` |
| Pos-implantacao | `Finalizar <ID>` |

Comandos legados ainda validos: `Iniciar AGROLEG-nnn`, `Analisar impacto AGROLEG-nnn` (equivalem a Fase 1).

---

## Fase 1 — Inicializacao + impacto preliminar

**Disparo:** `Implementar AGROLEG-nnn.xml`

### Passos

1. Localizar XML em `Cobol/Projetos/**/AGROLEG-nnn/AGROLEG-nnn.xml` ou caminho informado.
2. Criar pasta da demanda se nao existir: `Cobol/Projetos/<Consultoria>/<ID>/`.
3. Copiar README de `Cobol/Projetos/_indice/template-README-demanda.md` se ausente.
4. Sync Jira:
   ```powershell
   powershell -ExecutionPolicy Bypass -File ".cursor/skills/jira-xml-readme-sync/scripts/sync_jira_xml_readme.ps1" -XmlPath "<xml>" -ReadmePath "<readme>" -InPlace
   ```
5. Consultar `agroleg-knowledge` (`index.md`, `initiatives/`, `modules/`).
6. Preencher README: Objetivo, Requisito de negocio, Centros de custo, Escopo tecnico, Fontes impactados (preliminar), Historias relacionadas.
7. Montar `## Fontes pendentes baixa` (prioridade P1/P2/P3).
8. Se ja existirem fontes na pasta: rodar dependencias LIBS com `-InPlace`.
9. Atualizar `## Status workflow`: fase `Analise preliminar concluida — aguardando fontes`.

### Saida obrigatoria (relatorio)

```text
=== AGROLEG-nnn — INICIALIZACAO ===

NEGOCIO
- Objetivo:
- Iniciativa:
- Mensagens COR/CIR:

IMPACTO PRELIMINAR (acervo)
- Historias relacionadas:
- Suspeitos de impacto:

FONTES A BAIXAR
| Prioridade | Tipo | Member | Motivo | Destino |
| P1 | PGM/CPY/JCL/MFS | ... | ... | pgm/ cpy/ jcl/ mfs/ |

PROXIMO PASSO
Baixar fontes P1 e informar: "Fontes baixados — continuar analise AGROLEG-nnn"
```

---

## Fase 2 — Analise iterativa de fontes

**Disparo:** `Fontes baixados — continuar analise AGROLEG-nnn`

### Passos (cada rodada)

1. Ler fontes em `pgm/`, `cpy/`, `jcl/`, `mfs/` da demanda.
2. Extrair COPY, EXEC SQL INCLUDE, CALL, dependencias cruzadas.
3. Cruzar com `Cobol/LIBS`, acervo e README de historias relacionadas.
4. Rodar dependencias LIBS `-InPlace` se houver fontes.
5. Atualizar `## Fontes impactados (preliminar)` e `## Fontes pendentes baixa`.
6. Se SICOR: consultar `initiatives/SICOR-5.12.md`.
7. Se SQL: rule `dclgen-sql-validation`.
8. Se CICS: considerar `cics-bms-maps`.
9. Se BATCH/JCL: skill `malha-batch-analise` — JSON em `referencia-batch-malha/malhas/` + HTML visual.

### Conclusao da rodada (uma das duas)

**A) ANALISE PARCIAL — faltam fontes**

```text
=== AGROLEG-nnn — ANALISE PARCIAL ===

FONTES ANALISADAS
- (lista)

FONTES FALTANTES
| Tipo | Member | Motivo (COPY/CALL/referencia) | Prioridade |

PROXIMO PASSO
Baixar fontes faltantes e informar: "Fontes baixados — continuar analise AGROLEG-nnn"
```

Atualizar README: `## Status workflow` → `Analise em andamento — aguardando fontes`.

**B) ANALISE CONCLUIDA**

```text
=== AGROLEG-nnn — ANALISE CONCLUIDA ===

ESCOPO CONFIRMADO
- Programas:
- Copybooks:
- JCL/MFS:
- Malha batch (se aplicavel): link HTML em `## Malha batch`
- Tabelas DCLGEN:

RISCOS
- ...

PROXIMO PASSO
Informar: "Gerar plano de ajuste AGROLEG-nnn"
```

Atualizar README:
- `## Impacto tecnico (confirmado)` (escopo analisado — refinado na implementacao)
- `## Fontes pendentes baixa`: Nenhuma
- `## Status workflow`: `Analise concluida — aguardando plano de ajuste`

**Gate:** nao avancar para Fase 3 sem declarar `ANALISE CONCLUIDA`.

---

## Fase 3 — Plano de ajuste

**Disparo:** `Gerar plano de ajuste AGROLEG-nnn`

**Pre-requisito:** Fase 2 concluida.

### Passos

1. Com base na analise, montar `## Plano de ajuste (proposta)` no README.
2. Incluir ordem de implementacao, riscos e cenarios de teste preliminares.
3. Referenciar regras aplicaveis (SICOR 5.12, quality code, style).

### Formato do plano (README)

```markdown
## Plano de ajuste (proposta)
| # | Fonte | Area/Section | Alteracao proposta | Referencia |
| 1 | ... | ... | ... | SICOR 5.12 / quality #10 / ... |

### Ordem de implementacao
1. ...

### Riscos
- ...

### Cenarios de teste (preliminar)
- ...
```

### Saida obrigatoria

```text
=== AGROLEG-nnn — PLANO DE AJUSTE ===

(resumo do plano)

GATE
Revise o plano no README. Para implementar, responda:
"Implementar plano AGROLEG-nnn"
```

Atualizar `## Status workflow`: `Plano proposto — aguardando validacao`.

**Gate:** implementacao **somente** apos usuario dizer `Implementar plano AGROLEG-nnn`.

---

## Fase 4 — Desenvolvimento

**Disparo:** `Implementar plano AGROLEG-nnn`

**Pre-requisito:** plano validado explicitamente pelo usuario.

### Passos

1. Executar plano na ordem definida.
2. Seguir `Cobol/Projetos/_indice/referencia-cobol/` (caps. 01–09) + skills:
   - `bradesco-style`, `bradesco-quality-code`
   - `cics-bms-maps` (se CICS)
   - `initiatives/SICOR-5.12.md` (se SICOR)
   - rule `dclgen-sql-validation` (se SQL)
3. Alteracoes: **comentar linha antiga + criar linha nova** (cap. 03); verbos comentados com `-`; **nao excluir linhas**.
4. Programa novo: 1000-INICIALIZAR / 2000-PROCESSAR / 3000-FINALIZAR (cap. 01).
5. Indentacao trecho novo: verbo col 20, complemento col 40 (cap. 01).
6. Atualizar README: `## Decisoes tecnicas`, `## Impacto tecnico (confirmado)`.
7. Auto-verificacao cap. 03 nos fontes alterados: verbos comentados com `-`; prefixo da demanda atual (nao historico); ver `bradesco-quality-code` secao "Prefixo historico vs demanda atual".
8. Atualizar `## Status workflow`: `Implementacao concluida — aguardando pre-subida`.

### Saida obrigatoria

```text
=== AGROLEG-nnn — IMPLEMENTACAO CONCLUIDA ===

FONTES ALTERADOS
- ...

PROXIMO PASSO
Informar: "Pre-subida AGROLEG-nnn"
```

---

## Fase 5 — Pre-subida

**Disparo:** `Pre-subida AGROLEG-nnn`

### Passos

1. Acionar skill `demanda-pre-subida`.
2. Corrigir achados **BLOQUEANTE** com `bradesco-quality-code`.
3. Se REPROVADO: nao marcar pronto para subida; listar acoes.
4. Se APROVADO: atualizar Status README.

Atualizar `## Status workflow`: `Pre-subida aprovada — aguardando subida MF`.

### Saida

Relatorio `demanda-pre-subida` + orientacao: usuario valida e sobe para MF.

---

## Fase 5b — Subida MF (Changeman)

**Disparo:** usuario informa subida/compilacao, ou ao concluir pre-subida aprovada.

**Pre-requisito:** Fase 5 aprovada (ou usuario confirma subida direta).

### Informacoes obrigatorias (solicitar ao usuario)

Antes de atualizar README e avancar para homologacao, **obter**:

1. **Pacote de subida** — numero Changeman (ex.: `RUCA000752`), members incluidos, status do compile (RC=0).
2. **Pacote de contingencia** — numero Changeman (ex.: `RUCA000753`) com versao **PRD pre-demanda** (baseline em `pgm/analise/`, `cpy/analise/`) para retorno pos-implantacao.
3. **Ticket Jira de promocao** — chave (ex.: `TICKET-85280`) e link browse para promocao dos pacotes entre ambientes (TST / HOM / PRD).

Se o usuario ainda nao criou o pacote de contingencia, orientar a monta-lo com os fontes baseline **antes** da implantacao em PRD.

### Passos

1. Solicitar numeros dos pacotes (subida + contingencia), members, resultado do compile e ticket Jira de promocao.
2. Atualizar README:
   - `## Pacotes changeman` (tabela completa)
   - `## Promocao ambientes (Jira)` (ticket, link, pacotes associados, status)
   - `## Evidencias` (registrar pacotes e ticket)
   - `## Status workflow`: `Compilado Changeman — aguardando homologacao`
3. Marcar checkbox `Pronto para subida` e `Em homologacao` em `## Status`.

### Saida obrigatoria

```text
=== AGROLEG-nnn — SUBIDA MF ===

PACOTES CHANGEMAN
- Subida:       RUCA000xxx — members — compilado OK
- Contingencia: RUCA000yyy — baseline PRD

PROMOCAO AMBIENTES (JIRA)
- Ticket: TICKET-xxxxx — https://jira.bradesco.com.br:8443/browse/TICKET-xxxxx

PROXIMO PASSO
Homologacao funcional (Fase 6)
```

---

## Fase 6 — Homologacao (humana)

**Responsavel:** usuario.

O agente **nao altera fontes** nesta fase. Aguardar informacao do usuario.

Atualizar README quando usuario informar:
- `## Status workflow`: `Em homologacao` ou `Implantado`

Checkboxes Status:
```markdown
- [x] Em andamento
- [x] Em teste
- [x] Pronto para subida
- [x] Em homologacao
- [ ] Implantado
```

---

## Fase 7 — Encerramento (pos-homologacao)

**Disparo:** `Finalizar AGROLEG-nnn`

**Pre-requisito:** homologacao concluida e implantacao confirmada pelo usuario.

### Passos

1. Completar README:
   - `## Impacto tecnico (confirmado)`
   - `## Decisoes tecnicas`
   - `## Historias relacionadas`
   - `## Licoes para proximas analises`
   - `## Evidencias`
2. Atualizar `agroleg-knowledge/modules/<PREFIXO>.md`.
3. Se SICOR 5.12: atualizar `initiatives/SICOR-5.12.md`.
4. Regenerar indice:
   ```powershell
   powershell -ExecutionPolicy Bypass -File ".cursor/skills/agroleg-knowledge/scripts/rebuild_index.ps1" -ProjetosPath "Cobol/Projetos"
   ```
5. Atualizar `Cobol/Projetos/_indice/catalogo-demandas.md`.
6. Mover para `Finalizada/` se for politica do projeto (somente se usuario pedir).
7. Status: `Implantado` / `Finalizada`; `## Status workflow`: `Encerrada`.

### Saida obrigatoria

```text
=== AGROLEG-nnn — ENCERRAMENTO ===

ACERVO ATUALIZADO
- Modulos:
- Iniciativa:
- Indice regenerado

DEMANDA ENCERRADA
```

---

## Skills filhas por tipo

| Tipo | Skills / referencias |
|---|---|
| Qualquer COBOL | `bradesco-style`, `bradesco-quality-code` |
| SICOR 5.12 | `agroleg-knowledge/initiatives/SICOR-5.12.md` |
| CICS/BMS | `cics-bms-maps` |
| SQL/DB2 | rule `dclgen-sql-validation`, `readme-dependencias-libs` |
| BATCH / JCL | `malha-batch-analise`, `referencia-batch-malha/` |
| Conhecimento | `agroleg-knowledge` |
| Pre-subida | `demanda-pre-subida` |
| README | `readme-demanda-template`, `jira-xml-readme-sync`, `demanda-intake-reuniao` |

## Estrutura de pastas da demanda

Capgemini (por CC): `Cobol/Projetos/Capgemini/AGROLEG-nnn/<CENTRO_CUSTO>/COBOL|JCL|...`

Stefanini / 7Comm (por componente):

```
Cobol/Projetos/<Consultoria>/<ID>/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/  mfs/  db2/
└── <componente>/analise/   (fontes so para analise — nao alterar)
```

- Escopo: `pgm/`, `cpy/`, `jcl/`
- Analise: `pgm/analise/`, `cpy/analise/`, etc.
- Malha: `net/` (exports LIG*, schedule)

Ver `Cobol/Projetos/_indice/guia-organizacao-seguranca.md`.
