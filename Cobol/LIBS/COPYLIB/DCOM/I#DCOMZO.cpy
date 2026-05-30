      ******************************************************************
      * NOME BOOK : I#DCOMZO                                           *
      * DESCRICAO : LAYOUT DO ARQUIVO DE ALTERACAO BVVE                *
      * DATA      : 09/10/2020                                         *
      * AUTOR     : FELIPE MENDES                                      *
      * EMPRESA   : CAPGEMINI BRASIL                                   *
      * TAMANHO   : 157 BYTES                                          *
      ******************************************************************
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CANAL.........: CANAL SOLICITANTE                             *
      *  CONTRATO......: NUMERO DO CONTRATO/OPERACAO                   *
      *  IND-I-A.......: INDICADOR DE INCLUSAO 'I' OU ALTERACAO 'A'    *
      *  DT CONT.......: DATA DE VENCIMENTO DO CONTRATO (DD.MM.AAAA)   *
      *  VLR TOT.......: VALOR TOTAL DO CONTRATO                       *
      *  QTD-AGENDAS...: QTD DE AGENDAS VINCULAS A OPERACAO            *
      *  PROTOCOLO.....: CODIGO DO PROTOCOLO                           *
      *                                                                *
      *  PRODUTO.......: PRODUTO PARA MARCACAO                         *
      *  DATA..........: DATA DO RECEBIVEL PARA MARCACAO (DD.MM.AAAA)  *
      *  VALOR CESSAO..: VALOR PARA MARCACAO (CESSAO)                  *
      *                                                                *
      ******************************************************************
       05  DCOMZO-REGISTRO.
           10 DCOMZO-E-AGENCIA                         PIC  9(005).
           10 DCOMZO-E-CONTA                           PIC  9(013).
           10 DCOMZO-E-CANAL                           PIC  X(004).
           10 DCOMZO-E-CONTRATO                        PIC  X(019).
           10 DCOMZO-E-IND-I-A                         PIC  X(001).
           10 DCOMZO-E-DT-CONT                         PIC  X(010).
           10 DCOMZO-E-VLR-TOT                         PIC  9(015)V99.
           10 DCOMZO-E-QTD-AGENDAS                     PIC  9(005).
           10 DCOMZO-E-PROTOCOLO                       PIC  X(050).
           10 DCOMZO-E-PRODUTO                         PIC  9(005).
           10 DCOMZO-E-DATA                            PIC  X(010).
           10 DCOMZO-E-VLR-CESSAO                      PIC  9(015)V9(2).
