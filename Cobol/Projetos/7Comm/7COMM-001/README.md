# 7COMM-001 - Correcao chamada GRUP (GRUP0930 → GRUP201C) na malha SLIG

## Status workflow
- Fase atual: Implementacao — escopo GRUP concluido nos 10 PGMs
- Ultimo comando: Implementar correcoes GRUP0930 → GRUP201C
- Proximo passo: Teste dos PGMs GRUP na cadeia; padronizar literais GRUP201C em displays (opcional)

## Objetivo
Corrigir a **carga do codigo de GRUPO** na malha batch SLIG apos CNPJ alfanumerico: substituir chamadas ao modulo **GRUP0930** pelo clone **GRUP201C**, ajustando a interface **GRUPW01S** (CNPJ `PIC X(11)` + filial `PIC X(04)`) nos programas listados pelo Bradesco.

## Requisito de negocio
- Iniciativa: CNPJ alfanumerico (contexto SLIG — modulo GRUP)
- Mensagens COR/CIR desta historia: N/A
- Regra resumida: Programas SLIG que consultam grupo economico devem chamar **GRUP201C** com mapeamento correto de CNPJ/filial alfa.

## Centros de custo impactados
- SLIG

## Escopo desta entrega (apenas GRUP)

| De | Para | Copybook |
|---|---|---|
| GRUP0930 | GRUP201C | GRUPW01S (substitui I#GRUP34) |

**Interface GRUP201C (padrao SLIG6102):**
- `MOVE '00' TO GRUPW01S-CGC-CPF(1:2)`
- Raiz CNPJ em `GRUPW01S-CGC-CPF(3:9)` a partir do campo `-ST` do layout
- Filial em `GRUPW01S-FILIAL-CGC-CPF` via `FILIAL-ST(2:4)`
- Controle em `GRUPW01S-CTRL-CGC-CPF`

## Fora de escopo (nao alterar nesta demanda)

| Item | Observacao |
|---|---|
| I#CBON01 / CNPJAN | Outra entrega — nao mexer |
| I#DEVE17 → I#DEVEN7 | Outra entrega — nao mexer |
| CLIB2000 → CLIBB000 | Outra entrega — nao mexer |
| JCL / LRECL / SORT | Fora desta entrega GRUP |
| Demais PGMs da malha SLIG | Apenas os 10 abaixo |

## Estrutura de pastas (componentes)
- `pgm/` — programas do escopo GRUP
- `cpy/` — copybook GRUPW01S (pendente baixa)
- `jcl/`, `net/` — referencia de malha (nao alterar nesta entrega)

## Programas do escopo

Demanda formal: e-mail Bradesco 28/05/2026 (`RES_ SLIG - ERRO na carga do código do GRUPO (novo modulo GRUP201C).msg`).

| PGM | Pacote | Status local |
|---|---|---|
| SLIG6100 | SLIG000873 | **corrigido** — GRUP201C + GRUPW01S |
| SLIG6102 | SLIG000887 | OK (referencia de mapeamento) |
| SLIG6103 | SLIG000887 | **corrigido** — prefixo `'00'` no CNPJ |
| SLIG6105 | SLIG000891 | **corrigido** — CNPJ(11) + filial(2:4) |
| SLIG6211 | SLIG000888 | OK |
| SLIG6226 | SLIG000888 | OK |
| SLIG6210 | SLIG000926 | OK |
| SLIG6215 | SLIG000926 | OK |
| SLIG6216 | SLIG000926 | **corrigido** — modulo GRUP201C (era GRUPW01S) |
| SLIG6420 | SLIG000926 | **corrigido** — GRUP201C (era GRUP0930) |

## Fontes pendentes baixa

| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | CPY | GRUPW01S | interface GRUP201C | cpy/ | **pendente baixa** |
| P2 | CPY | I#GRUP34 | referencia legado (comparar) | cpy/analise/ | opcional |

## Impacto tecnico (confirmado — GRUP)

- **GRUP0930:** modulo numerico, area I#GRUP34 / REG-EMPRESA-GRUPO-GP34
- **GRUP201C:** clone CNPJ alfa, area GRUPW01S
- **SLIG6100:** era o principal gap — ainda chamava GRUP0930; migrado para GRUP201C
- **SLIG6105:** mapeamento incorreto truncava filial; alinhado ao SLIG6102
- **SLIG6216:** bug — CALL apontava para nome do copybook (`GRUPW01S`)
- **SLIG6420:** usava GRUPW01S na area mas CALL ainda em GRUP0930

## Plano de ajuste (executado)

1. Migrar SLIG6100: COPY GRUPW01S, CALL GRUP201C, mapeamento -ST
2. Corrigir SLIG6105: CNPJ X(11) + filial X(04)
3. Corrigir SLIG6420 e SLIG6216: modulo GRUP201C
4. Revisar SLIG6103: prefixo `'00'` no CNPJ
5. Validar SLIG6102, 6210, 6211, 6215, 6226 (ja conformes)

## Decisoes tecnicas
- Escopo limitado a **GRUP0930 → GRUP201C** — demais modulos (CBON, DEVE, CLIB) permanecem como estao nos fontes
- Referencia de implementacao: **SLIG6102**
- Literais `(GRUP0930)` em totais/display: cosmeticos; atualizar se solicitado na homologacao

## Historias relacionadas

| Historia | Relacao | Observacao |
|---|---|---|
| STEF-001 | mesma-iniciativa | CNPJ alfa em outro centro de custo (GFCT) |

## Dependencias LIBS
- COPYBOOKS: GRUPW01S (escopo); demais copybooks dos PGMs — apenas leitura, sem alteracao
- DCLGEN: N/A

## Riscos e atencoes
- Copybook GRUPW01S ainda nao baixado em `cpy/` — necessario para compilacao local
- PGMs GRUP convivem com alteracoes CNPJ de outras entregas nos mesmos fontes — nao reverter trechos nao-GRUP

## Plano de testes
- [ ] SLIG6100 — carga codigo grupo cliente (GRUP201C)
- [ ] SLIG6105 — mapeamento CNPJ/filial sem truncamento
- [ ] SLIG6103 — grupo sacado
- [ ] SLIG6420 — grupo fornecedor/comprador
- [ ] SLIG6210/6211/6215/6216/6226 — regressao chamada GRUP201C

## Evidencias
- `RES_ SLIG - ERRO na carga do código do GRUPO (novo modulo GRUP201C).msg`
- net/LIG1001.txt — contexto de malha (referencia)

## Dados de origem
- Consultoria: 7Comm
- Canal: E-mail Bradesco (encaminhado 28/05/2026)
- Referencia externa: Edinelson Pacci / Aline Coutinho — erro carga GRUPO SLIG
- Data intake: 2026-05-26
- Escopo refinado: 2026-05-28 — **somente GRUP**

## Status
- [x] Em andamento
- [ ] Em teste
- [ ] Pronto para subida
- [ ] Em homologacao
- [ ] Implantado
