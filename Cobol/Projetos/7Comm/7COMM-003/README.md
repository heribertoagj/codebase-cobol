# 7COMM-003 - CNPJ SACADO zerado — SLIGB010 (CLI_CHEQ_CONSL)

## Status workflow
- Fase atual: Plano proposto — aguardando validacao
- Ultimo comando: Gerar plano de ajuste 7COMM-003
- Proximo passo: Revisar plano abaixo e, se OK, `Implementar plano 7COMM-003`

## Objetivo
Corrigir a **carga diaria** da tabela **SLIGB010** (`CLI_CHEQ_CONSL` / `DB2PRD.DEPDC_BANDA_MAGNT`), cujos registros estao com **CNPJ SACADO zerado** (`CCGC_CPF_SACDO_ST`, filial e controle), no contexto da iniciativa **CNPJ alfanumerico** no modulo de **cheques SLIG**.

## Requisito de negocio
- Iniciativa: CNPJ alfanumerico — SLIG Cheques (ver 7COMM-001)
- Mensagens COR/CIR desta historia: N/A
- Regra resumida: A carga diaria de `CLI_CHEQ_CONSL` deve popular corretamente os campos de identificacao do **sacado**; hoje todos os registros apresentam CNPJ zerado apesar de `CINDCD_PSSOA_SACDO` preenchido (F/J).

## Centros de custo impactados
- SLIG

## Escopo tecnico
- COBOL BATCH: **SLIG6224 → SLIG6801 → SLIG4760** (carga SLIGB010); upstream **SLIG6210** (EMITCHEQ)
- JCL: SLIGC105, SLIGC182, SLIGC184 (+ SLIG1215 upstream)
- DB2: `DB2PRD.DEPDC_BANDA_MAGNT` — LOAD REPLACE via `MX.DBII.DEFUTIL(SLIG#010)`
- CICS/IMS: N/A

## Malha batch

### Trilha carga SLIGB010 (escopo)

```
MX.SLIG.EMITCHEQ(0)
  → SLIGC105 / SLIG6224 → CONSCHEC (I#SLIG43, 300 bytes)
  → SLIGC182 / SORT → DEPBANSR
  → SLIGC182 / SLIG6801 → DEPBANDA (I#SLIG5B, 146 bytes)
  → SLIGC182 / SLIG4760 → merge DEPBNDAS (historico)
  → SLIGC182 / SORT → MX.SLIG.JC182S03.DEPBANDA
  → SLIGC184 / DB2 LOAD → SLIGB010
```

### Upstream EMITCHEQ

```
SLIG1215 / SLIG6210: MVTCHQSR (I#SLIG27) → EMITCH01 (I#SLIG28) — popula EMITT-ST
SLIG1216 / SLIG6216: enriquece CLIENT em EMITCHEQ (nao reescreve EMITT)
SLIGC105 / SLIG6224: EMITCHEQ → CONSCHEA/CONSCHEC (consolidado emitente)
```

### Fora do escopo desta demanda
- SLIG7110/7120 (JCLs usam PLAN2440/SLIG9990 — trilha MVTOCHDE/reprocesso)
- Utilitario LOAD SLIG#010 (card DB2 — sem alteracao salvo mapeamento)

## Fontes impactados (confirmado)

| Member | Papel | Alteracao |
|---|---|---|
| **SLIG6801** | Gera DEPBANDA; sacado ← emitente (SLIG43) | **Provavel** |
| **SLIG6224** | EMITCHEQ → CONSCHEC (SLIG43) | **Provavel** (propagacao EMITT-ST) |
| **SLIG6210** | Popula EMITT-ST no EMITCHEQ a partir de SLIG27/CLIB | **Possivel** (upstream) |
| SLIG4760 | Reformata historico DEPBNDAE; F/J → filial 99991/99992 | Revisar (nao explica CNPJ raiz zerado) |
| SLIG6215/6216/6218 | Cadeia EMITCHEQ | Analise se 6210 insuficiente |
| SLIG6230 | Upstream decenal | Referencia (P2 — nao baixado) |

### Copybooks / DCLGEN
- **I#SLIG5B** — layout DEPBANDA / registro LOAD (146 bytes)
- **I#SLIG43** — CONSCHEC / DEPBANSR (300 bytes)
- **I#SLIG28** — EMITCHEQ (300 bytes)
- **I#SLIG27** — MVTCHQSR (300 bytes)
- **SLIGB010** — DCLGEN tabela alvo (`Cobol/LIBS/DCLGEN/SLIG/sligb010.cpy`)

## Estrutura de pastas (componentes)

- `pgm/` — SLIG6801, SLIG6224, SLIG4760, SLIG6210 (+ referencia)
- `jcl/` — SLIGC105, SLIGC182, SLIGC184, SLIG1215
- `net/` — dumps (h140624.txt)

## Fontes pendentes baixa

Nenhuma P1 pendente.

| Prioridade | Tipo | Member | Motivo | Status |
|---|---|---|---|---|
| P2 | PGM | SLIG6200 | SLIG1210 upstream DECECHEQ | opcional |
| P2 | PGM | SLIG6290 | SLIG1218 upstream | opcional |

Demais fontes P1: **baixados** (pgm/, jcl/).

## Impacto tecnico (confirmado)

### Regra de negocio tecnica (banda magnetica cheques)
Na carga SLIGB010, **sacado = emitente do cheque** — nao e o cliente/mutuario. Confirmado em `SLIG6801` section `3100-GRAVAR-DEPBANDA`:

```510:519:codebase-cobol/Cobol/Projetos/7Comm/7COMM-003/pgm/slig6801.cbl
CNPJ7C     MOVE SLIG43-CGC-CPF-EMITT       TO SLIG5B-CGC-CPF-SACDO
...
           MOVE SLIG43-CGC-CPF-EMITT-ST  TO SLIG5B-CGC-CPF-SACDO-ST.
CNPJ7C     MOVE ZEROS                    TO SLIG5B-CFLIAL-SACDO-ST
CNPJ7C     MOVE SLIG43-FILIAL-EMITT-ST   TO SLIG5B-CFLIAL-SACDO-ST(2:4)
BRQ250     MOVE SLIG43-CONTROLE-EMITT-ST TO SLIG5B-CCTRL-SACDO-ST
           MOVE SLIG43-TIPO-PESSOA-EMITT
                                        TO SLIG5B-INDCD-PSSOA-SACDO.
```

### Causa raiz (analise)
1. **`CINDCD_PSSOA_SACDO` preenchido (F/J)** vem de `SLIG43-TIPO-PESSOA-EMITT` — fluxo ate SLIG6801 funciona para tipo pessoa.
2. **`CCGC_CPF_SACDO_ST` zerado** indica `SLIG43-CGC-CPF-EMITT-ST` vazio ao gravar DEPBANDA.
3. **SLIG6224** (`3100-FORMATAR-CAMPOS`) copia `SLIG28-CGC-CPF-EMITT-ST` → `SLIG43-CGC-CPF-EMITT-ST`, mas **nao** popula campos numericos `SLIG43-CGC-CPF-EMITT` / filial / controle numericos.
4. **SLIG6210** popula `SLIG28-CGC-CPF-EMITT-ST` a partir de `SLIG27-CGC-CPF-EMITT-ST` e CLIB2000 (`NUM-CGCCPF-ST`) — ponto upstream se EMITCHEQ chega sem EMITT-ST.
5. **CNPJ alfanumerico:** tags `CNPJ7C` em SLIG6801/6218 indicam alteracoes parciais; se EMITT-ST nao propagar da cadeia SLIG27→6210→6224→6801, LOAD grava zeros nos campos `_ST` da SLIGB010.
6. **SLIG4760** zera `CCTRL-SACDO-ST` e move 99991/99992 em filial para CPF (F/J) no **historico** — nao corrige CNPJ raiz zerado; explica padrao legado em filial/controle.

### Fluxo resumido do defeito

```
SLIG27 EMITT-ST vazio ou numerico zerado
  → SLIG6210 EMITT-ST vazio no EMITCHEQ
  → SLIG6224 SLIG43-EMITT-ST vazio
  → SLIG6801 SLIG5B-CGC-CPF-SACDO-ST vazio
  → LOAD SLIGB010 com CCGC_CPF_SACDO_ST = zeros
  (CINDCD_PSSOA_SACDO ok via TIPO-PESSOA-EMITT)
```

### Escopo de correcao (confirmado na analise)
Ver `## Impacto tecnico (confirmado)` — sacado = emitente; defeito na propagacao de `-ST`.

## Plano de ajuste (proposta)

Objetivo: garantir que `CCGC_CPF_SACDO_ST`, `CFLIAL_SACDO_ST` e `CCTRL_CGC_SACDO_ST` cheguem preenchidos no arquivo DEPBANDA (I#SLIG5B) antes do LOAD SLIGB010, propagando CNPJ alfanumerico do emitente do cheque.

| # | Fonte | Area / Section | Alteracao proposta | Referencia |
|---|---|---|---|---|
| 1 | slig6210.cbl | `310000-GUARDAR-CAMPOS` | Apos MOVE de `SLIG27-CGC-CPF-EMITT-ST`, se `WRK-CGC-CPF-EMITT` = SPACES / LOW-VALUES / `'000000000'`, tentar `SLIG27-CGC-CPF-EMITT` (numerico) ou manter ST da entrada | cap. 03 — comentar + nova linha |
| 2 | slig6210.cbl | `321000-OBTER-DADOS-EMITENTE` | Garantir preenchimento de `WRK-CGC-CPF-EMITT` / filial / controle a partir de `NUM-CGCCPF-ST` / `FILIAL-CGCCPF-ST` / `CONTR-CGCCPF-ST` do CLIB2000 quando ST da SLIG27 vazio; nao zerar emitente se CLIB falhar mas ST da SLIG27 existir | cap. 03; padrao CNPJ alfa SLIG |
| 3 | slig6210.cbl | `1930-1940` (tipo pessoa emitente) | Trocar testes `WRK-CGC-CPF-EMITT EQUAL ZEROS` por condicao PIC X: SPACES, LOW-VALUES ou `'000000000'` (alfa numerico invalido ≠ vazio) | quality code — comparacao PIC X |
| 4 | slig6210.cbl | `320000-GRAVAR-EMITCHEQ` | Antes do WRITE: assegurar `SLIG28-CGC-CPF-EMITT-ST` / filial / controle `-ST` gravados a partir de WRK; nao sobrescrever ST valido com zeros | cap. 03 |
| 5 | slig6224.cbl | `3100-FORMATAR-CAMPOS` | Apos copias `-ST` existentes, incluir MOVE de `SLIG28-CGC-CPF-EMITT` / `FILIAL-EMITT` / `CONTROLE-EMITT` → `SLIG43-*` (campos numericos) para compatibilidade com bloco CNPJ7C do SLIG6801 | cap. 03 |
| 6 | slig6224.cbl | `3100-FORMATAR-CAMPOS` | Se `SLIG28-CGC-CPF-EMITT-ST` = SPACES / LOW-VALUES / `'000000000'`, nao INITIALIZE apagar recuperacao — validar entrada EMICHESR (display contador ou mensagem SRELCTRL) | cap. 01 — evidencia |
| 7 | **slig6801.cbl** | **`3100-GRAVAR-DEPBANDA`** | **Principal:** antes do WRITE, validar `SLIG43-CGC-CPF-EMITT-ST`; mover para `SLIG5B-CGC-CPF-SACDO-ST`; ajustar filial ST: `MOVE '00' TO SLIG5B-CFLIAL-SACDO-ST(1:2)` + `MOVE SLIG43-FILIAL-EMITT-ST TO SLIG5B-CFLIAL-SACDO-ST(2:4)` (substituir MOVE ZEROS + partial) | cap. 03; I#SLIG5B / SLIGB010 |
| 8 | slig6801.cbl | `3100-GRAVAR-DEPBANDA` | Bloco CNPJ7C numerico (`SLIG5B-CGC-CPF-SACDO`): so popular de `SLIG43-CGC-CPF-EMITT` quando numerico valido; se alfa, manter numerico zerado (carga via colunas `_ST`) | cap. 03 |
| 9 | slig6218.cbl | `320000-GRAVAR-EMITCHEQ` | Revisar bloco CNPJ7C (linhas 668–677): quando CNPJ alfa, nao zerar campos numericos desnecessariamente se `WRK-CPFCGC-EMI` (ST) ja preenchido; garantir `SLIG28-CGC-CPF-EMITT-ST` antes do WRITE | cap. 03 — ramo SLIG1218 → EMITCHEQ |
| 10 | slig4760.cbl | `3100-FORMATAR-DEPBNDAS` | **Nao alterar nesta entrega** — regra 99991/99992 e legado CPF no historico DEPBNDAE; registrar evidencia pos-teste | fora escopo v1 |

### Ordem de implementacao

1. **SLIG6210** — origem EMITT-ST no EMITCH01/EMITCHEQ (items 1–4).
2. **SLIG6218** — ramo paralelo EMITCHEQ via SLIG1218 (item 9).
3. **SLIG6224** — consolidacao CONSCHEC (items 5–6).
4. **SLIG6801** — gravacao DEPBANDA / imagem SLIGB010 (items 7–8).
5. Teste integrado TU: SLIG1215 → SLIGC105 → SLIGC182 → SLIGC184 (sob demanda).
6. **SLIG4760** — somente se teste indicar regressao no merge historico (item 10).

### Riscos do plano

- Alteracao em **SLIG6210** compartilhado com **7COMM-001** — validar regressao GRUP201C / malha EMITCHEQ.
- **Sacado = emitente** (nao cliente): nao corrigir campos CLI pensando ser sacado.
- CNPJ **alfanumerico** no emitente: comparacoes `EQUAL ZEROS` em PIC X podem mascarar valor valido — padronizar testes.
- **SLIG4760** pode manter filial 99991/99992 em registros historicos CPF — nao confundir com registros novos do SLIG6801.
- LOAD SLIG#010: confirmar mapeamento colunas `_ST` no card (sem alteracao prevista).

### Cenarios de teste (preliminar)

| # | Cenario | Validacao |
|---|---|---|
| 1 | Carga completa TU (malha sob demanda) | SELECT DEPDC_BANDA_MAGNT: `CCGC_CPF_SACDO_ST` <> zeros; `CINDCD_PSSOA_SACDO` coerente com F/J |
| 2 | Emitente CNPJ numerico (J) | Raiz + filial + controle `_ST` preenchidos |
| 3 | Emitente CNPJ alfanumerico | `CCGC_CPF_SACDO_ST` com valor alfa; numerico legado pode ser zero |
| 4 | Emitente CPF (F) | Filial ST conforme regra (99991 ou filial real pos-6801) |
| 5 | Regressao EMITCHEQ / CONSCHEC | Dump arquivo antes do LOAD — EMITT-ST presente no SLIG43 |
| 6 | Regressao 7COMM-001 | SLIG6210 — GRUP201C e EMITCHEQ sem quebra |

## Decisoes tecnicas
- Carga SLIGB010 = utilitario DB2 LOAD (SLIGC184 / SLIG#010) — confirmado.
- Sacado na SLIGB010 = emitente do cheque (SLIG6801) — confirmado.
- Rotinas batch em TU sob demanda — CONSCHEC vazio sem execucao e esperado.

## Historias relacionadas

| Historia | Relacao | Observacao |
|---|---|---|
| 7COMM-001 | mesma-iniciativa | CNPJ alfa SLIG — GRUP; SLIG6210 compartilhado |
| 7COMM-002 | mesmo-modulo | RUCA — escopo distinto |

## Licoes para proximas analises
- Job name ≠ PGM executado (SLIG7120 JCL = PLAN2440).
- Demanda SLIGB010: foco SLIGC105/182/184, nao trilha MVTOCHDE/CHEC.
- Verificar sempre quem gera arquivo **antes** do LOAD, nao o utilitario LOAD.

## Dependencias LIBS
- COPYBOOKS: I#SLIG27, I#SLIG28, I#SLIG43, I#SLIG5B, I#SLIG52, GRUPW01S, I#DEVE17
- DCLGEN: SLIGB010
- Pendencias PGM: SLIG6200, SLIG6290 (P2 opcional)
- Modulos: CLIB2000/CLIBB000, GRUP201C, POOL7100, PLAN0260

## Riscos e atencoes
- Correcao em SLIG6801 impacta **todos** os registros da carga diaria SLIGB010.
- SLIG4760 altera filial/controle para CPF (99991/99992) — nao quebrar legado numerico.
- Testar CNPJ emitente numerico e alfanumerico apos ajuste.
- Nao confundir sacado (emitente) com cliente na SLIGB010.

## Plano de testes
- [ ] SELECT DEPDC_BANDA_MAGNT — `CCGC_CPF_SACDO_ST` preenchido; `CINDCD_PSSOA_SACDO` coerente
- [ ] CNPJ emitente numerico e alfanumerico
- [ ] Regressao SLIGC105 → SLIGC182 → SLIGC184 (TU sob demanda)
- [ ] Validacao SICOR 5.12: N/A

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| | Subida (implementacao) | | pendente | pgm/ |
| | Contingencia (baseline PRD) | | pendente | pgm/analise/ |

## Evidencias
- E-mail + prints SQL: `email-origem.txt`, `anexos-email/`
- Dump TU MVTOCHDE: `h140624.txt`

## Dados de origem
- Consultoria: 7Comm
- Canal: E-mail — 2026-06-19 (Edinelson Pacci)
- Titulo: CNPJ SACADO zerado — SLIGB010 (CLI_CHEQ_CONSL)
- Atualizado em: 2026-06-19

## Status
- [x] Em andamento
- [ ] Em teste (dev interno — antes da subida MF)
- [ ] Pronto para subida (pre-subida aprovada)
- [ ] Em homologacao (gestor testa **apos** subida MF)
- [ ] Implantado (PRD — apos OK do gestor)
