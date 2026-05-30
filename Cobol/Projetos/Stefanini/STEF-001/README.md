# STEF-001 - Alteracao campos CNPJ e FILIAL nos programas GFCT

## Status workflow
- Fase atual: Analise em andamento
- Ultimo comando: Iniciar demanda Stefanini (intake)
- Proximo passo: Continuar analise de fontes em GFCT/

## Objetivo
Ajustar formato e consistencia dos campos CNPJ e FILIAL nos programas GFCT: PIC 9 para PIC X, substituicao POOL0110 por BRAD2000, movimentacao de variaveis com sufixo -ST e alteracoes em acessos DB2 nas tabelas listadas.

## Requisito de negocio
- Iniciativa: N/A
- Mensagens COR/CIR desta historia: N/A
- Regra de negocio resumida: Padronizar tratamento CNPJ/FILIAL alfanumerico e validacao via BRAD2000 conforme especificacao da consultoria.

## Centros de custo impactados
- GFCT

## Escopo tecnico
- COBOL: programas GFCT (PGM, 03.validar CNPJ, 06.outros, Programas_1711, Nao_iniciados)
- JCL: jobs em GFCT/JCL
- CICS: (a levantar)
- DB2: tabelas em GFCT/TABELAS e listadas em prompt.txt
- IMS: N/A
- BATCH: jobs GFCT

## Malha batch
- Fonte JSON: (a levantar se necessario)
- Visual HTML: (a levantar)

## Fontes impactados (preliminar)
- Programa(s): GFCT7500 (pedido.txt), demais em GFCT/PGM e subpastas
- Copybook(s): GFCTW2PJ, GFCTW3PJ e demais em GFCT
- DCLGEN(s): (a levantar via TABELAS)
- JCL(s): GFCT/JCL/*.jcl
- Mapa(s) BMS: N/A

## Fontes pendentes baixa
| Prioridade | Tipo | Member | Motivo | Destino | Status |
|---|---|---|---|---|---|
| P1 | PGM | GFCT7500 | pedido.txt | GFCT/ | a localizar |

## Impacto tecnico (confirmado)
Preencher quando analise estiver concluida (Fase 2).

- Programas alterados:
- Copybooks alterados:
- Tabelas DB2 (via DCLGEN): PDIDO_ESTRN_CLI, PDIDO_FLEXZ_CLI, SUSP_GRP_CLI, TDEB_UNFCA_CLI, TADSAO_GRP_CLI, TPRMSS_GRP_CLI, TCANCT_ADSAO_FORNC, TAUTRZ_ENVIO_CLI, TCTRL_NGTVC_CLI, TCLI_SEM_CESTA, FLEXZ_GRP_CLI, GSTAO_CLI_DESC, GSTAO_CLI_ESTRN, GSTAO_CLI_SERVC, V01CLIENTE_AGENCIA, V0OBJE_CLIENTES
- Fluxo resumido:

## Plano de ajuste (proposta)
Preencher na Fase 3. Regras detalhadas em `prompt.txt`.

## Decisoes tecnicas
- Comentar codigo antigo + nova linha com prefixo ST25X6 (ou prefixo existente do programa)
- CNPJ/CGC: PIC 9(09) -> PIC X(09); FILIAL: PIC 9(05) -> PIC X(04)
- POOL0110 -> BRAD2000

## Historias relacionadas
| Historia | Relacao | Observacao |
|---|---|---|
| STEF-002 | a confirmar | CEPT — verificar se mesmo escopo |

## Dependencias LIBS
- COPYBOOKS (encontradas): (rodar gerar_dependencias)
- DCLGEN (encontradas): (a levantar)
- Pendencias (nao encontradas em LIBS): (a levantar)

## Riscos e atencoes
- Grande volume de programas e JCLs em GFCT/
- Alterar apenas tabelas listadas em prompt.txt / GFCT/TABELAS

## Plano de testes
- [ ] Validacao BRAD2000 em programas alterados
- [ ] Regressao batch GFCT

## Evidencias
- Pacote CPY GFCT002943 / ADESAO GFCT002944 (ver Anotacoes)
- Mudanca: 000771339 | Sequencia: 2025-0038875-5-121

## Dados de origem
- Consultoria: Stefanini
- Canal: Reuniao / especificacao escrita (prompt.txt)
- Referencia externa: Sequencia 2025-0038875-5-121 | Mudanca 000771339
- Data intake: 2026-05-26

## Status
- [x] Em andamento
- [ ] Em teste
- [ ] Pronto para subida
- [ ] Em homologacao
- [ ] Implantado
