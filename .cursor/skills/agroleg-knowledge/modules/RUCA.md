# Modulo RUCA (Credito Rural — telas e consistencias)

## Papel no negocio

- Telas online de renegociacao, reorganizacao de contratos, consistencias de inclusao e calculos de vencimentos.

## Mapa tecnico

| Artefato | Papel | Observacao |
|---|---|---|
| RUCA7602 | Tela vencimentos contrato | AGROLEG-993 — arredondamento reneg/encargos |
| RUCA6690 | Dados contratos reorganizacao | AGROLEG-993 — PF8 "NAO HA MAIS REGISTROS" |
| RUCAW602 | Copybook area tela | Dependencia LIBS |

## Mensagens SICOR deste modulo

- Processos online RUCA podem gerar COR0005 (prorrogacao, manutencao mestres, produtividade, desclassificacao) — ver subtarefas AGROLEG-1239.

## Historias AGROLEG neste modulo

| Historia | Escopo | Relacao |
|---|---|---|
| AGROLEG-993 | Ajustes telas renegociacao e reorganizacao | base |
| AGROLEG-1109 | Impacto SICOR 5.12 (levantamento consolidado) | mesma-mensagem COR0001/COR0005 |
| AGROLEG-1239 | COR0005 — telas mantem campos excluidos da mensageria | complementa |

## Pontos sensiveis (impacto)

- Alteracoes de calculo em RUCA7602 afetam exibicao de valores renegociados — validar arredondamento.
- Paginacao PF8 em RUCA6690: tratar EOF com mensagem no rodape, nao tela em branco.
- Relacao com RUEC7951/RUEC7000 para fluxo completo de renegociacao.

## Licoes registradas

- AGROLEG-259 (regressao): Tipo Exploracao em RUEC7000 apos retorno da tela de imovel.
