# 7COMM-002 - Correcao CNPJ/CGC na tela RUCAT631 (RUCA6630)

## Status workflow
- Fase atual: Intake concluido — aguardando fontes
- Ultimo comando: Iniciar demanda 7Comm
- Proximo passo: Baixar fontes P1 do MF e informar `Fontes baixados — continuar analise 7COMM-002`

## Objetivo
Corrigir a **nao exibicao do CNPJ/CGC** na tela **RUCAT631** (Demonstracao de Situacao do Credito Rural), montada pelo programa online **IMS** **RUCA6630** apos consulta na tela de entrada **RUCAT630**.

## Requisito de negocio
- Iniciativa: A confirmar — possivel **CNPJ alfanumerico** (ver STEF-001, 7COMM-001)
- Mensagens COR/CIR desta historia: N/A
- Regra resumida: Ao consultar uma operacao rural, o campo **CGC/CPF** (linha 8 da RUCAT631) deve exibir o documento do mutuario corretamente.

## Centros de custo impactados
- RUCA

## Escopo tecnico
- COBOL IMS: **RUCA6630** (programa principal)
- MFS: **RUCAT631** (saida), **RUCAT630** (entrada)
- DB2: `DB2PRD.RUCA_CAD_ROOT` — DCLGEN **RUCAB003** (`B003_CGCPF1`)
- IMS: segmento cadastro CAD (area DB-CAD / RURA020)
- CICS: N/A
- BATCH/JCL: N/A

## Malha batch
- N/A (demanda online IMS)

## Fontes impactados (preliminar)
- Programa(s): RUCA6630
- Copybook(s): layouts CAD/HR03* (a confirmar na analise)
- DCLGEN(s): RUCAB003, RUCAB004 (referencia no cabecalho do PGM)
- JCL(s): N/A
- Mapa(s) MFS: RUCAT631, RUCAT630

## Estrutura de pastas (componentes)
- `pgm/` — RUCA6630 (escopo)
- `mfs/` — RUCAT631, RUCAT630 (escopo)
- `cpy/` / `db2/` — RUCAB003 e demais copybooks do escopo
- `pgm/analise/`, `mfs/analise/` — baseline PRD e referencias (RUCA7951, POOL0660)

Ver `Cobol/Projetos/_indice/guia-organizacao-seguranca.md`.

## Fontes pendentes baixa

| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | PGM | RUCA6630 | Programa IMS — monta RUCAT631 | pgm/ | pendente |
| P1 | MFS | RUCAT631 | Tela saida — campo CGCCPF | mfs/ | pendente |
| P1 | MFS | RUCAT630 | Tela entrada | mfs/ | pendente |
| P1 | DCLGEN | RUCAB003 | Cadastro root — B003_CGCPF1 | db2/ ou cpy/ | pendente |
| P2 | DCLGEN | RUCAB004 | Cadastro dep (referencia PGM) | db2/ ou cpy/analise/ | pendente |
| P2 | PGM | POOL0660 | Utilitario montagem mensagem IMS | pgm/analise/ | pendente |
| P2 | PGM | RUCA7951 | Referencia tratamento CNPJ (PIC X) | pgm/analise/ | pendente |
| P3 | CPY | HR03* | Bridge DB2→IMS CAD | cpy/analise/ | pendente |

**Referencia acervo (nao alterar):** `Capgemini/Finalizada/AGROLEG-001/RUCA/pgm/ruca6630.cbl`, `.../mfs/rucat631.mfs`

## Impacto tecnico (confirmado)
Preencher na Fase 2 apos analise dos fontes baixados do MF.

### Hipotese preliminar (acervo AGROLEG-001)
- Section `0070-MONTA-TELA-RUCAT631`: `MOVE CAD-CGCCPF1 TO WRK-DESC-CGC-R` → `MOVE WRK-NUMFILI TO OUT-CGC-CPF631`.
- `CAD-CGCCPF1` e `OUT-CGC-CPF631` sao **numericos** (`PIC 9` / `999999999/9999`); CNPJ alfanumerico pode nao exibir.
- `RUCA7951` usa abordagem com campos `PIC X(18)` na section `1230-OBTER-CPF-CNPJ` — candidato a padrao de correcao.

## Plano de ajuste (proposta)
Preencher na Fase 3 apos analise concluida.

## Decisoes tecnicas
- (a registrar na implementacao)

## Historias relacionadas

| Historia | Relacao | Observacao |
|---|---|---|
| AGROLEG-001 | mesmo-programa | Acervo RUCA6630/RUCAT631 em Finalizada — referencia |
| AGROLEG-993 | mesmo-modulo | RUCA6690 referencia RUCA6630 (PF6) |
| STEF-001 | mesma-iniciativa | CNPJ alfanumerico — centro GFCT |
| 7COMM-001 | mesma-iniciativa | CNPJ alfanumerico — centro SLIG |

## Licoes para proximas analises
- (a registrar ao encerrar)

## Dependencias LIBS
- COPYBOOKS (encontradas): N/A — aguardando fontes
- DCLGEN (encontradas): N/A
- Pendencias (nao encontradas em LIBS): RUCAB003, RUCAB004
- Dependencias de sistema: POOL0660, POOL0110, POOL0430, POOL0432, DIGITO

## Riscos e atencoes
- Confirmar nome do member: **RUCA6630** vs RUCA66630 citado inicialmente.
- Alteracao em layout MFS (OUT-TELA631 / MASC-RUCAT631) exige alinhamento com POOL0660.
- Programas chamados via PF keys (6682, 6650, 6690, 6655, 7951) recebem `WRK-I-CGCCPF` da area de tela — impacto cruzado a validar.

## Plano de testes
- [ ] Consulta operacao — CNPJ numerico exibido na RUCAT631
- [ ] Consulta operacao — CNPJ alfanumerico (se aplicavel)
- [ ] Regressao PF keys da RUCAT631 (PF4, PF6, PF7, PF8, PFE, etc.)
- [ ] Validacao SICOR 5.12: N/A

## Pacotes changeman

| Pacote | Tipo | Members | Status | Fontes workspace |
|--------|------|---------|--------|------------------|
| | Subida (implementacao) | | pendente | pgm/, mfs/, cpy/ |
| | Contingencia (baseline PRD) | | pendente | pgm/analise/, mfs/analise/ |

## Promocao ambientes (Jira)

| Ticket | Link | Pacote subida | Pacote contingencia | Status |
|--------|------|---------------|---------------------|--------|
| | | | | pendente |

## Evidencias
- (a registrar)

## Dados de origem
- Consultoria: 7Comm
- Canal: Reuniao / descricao analista — ver `intake.md`
- Referencia externa: N/A
- Chave Jira (Capgemini): N/A
- Titulo: Correcao CNPJ/CGC na tela RUCAT631 (RUCA6630)
- Status: Intake
- Responsavel: A definir
- Atualizado em: 2026-06-08

## Status
- [x] Em andamento
- [ ] Em teste (dev interno — antes da subida MF)
- [ ] Pronto para subida (pre-subida aprovada)
- [ ] Em homologacao (gestor testa **apos** subida MF)
- [ ] Implantado (PRD — apos OK do gestor)
