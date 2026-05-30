      ******************************************************************
      * NOME BOOK : I#DCOMZN                                           *
      * DESCRICAO : LAYOUT ARQUIVO DE CANCELAMENTO/LIQUIDACAO BVVE     *
      * DATA      : 09/10/2020                                         *
      * AUTOR     : FELIPE MENDES                                      *
      * EMPRESA   : CAPGEMINI BRASIL                                   *
      * TAMANHO   : 092 BYTES                                          *
      ******************************************************************
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CONTRATO......: NUMERO DO CONTRATO                            *
      *  CANAL.........: CANAL SOLICITANTE                             *
      *  IND-LIQ-CANC..: LIQUIDACAO "L" OU CANCELAMENTO "C"            *
      *  PROTOCOLO.....: CODIGO PROTOCOLO                              *
      *                                                                *
      ******************************************************************
       05  DCOMZN-REGISTRO.
           10 DCOMZN-AGENCIA                           PIC  9(005).
           10 DCOMZN-CONTA                             PIC  9(013).
           10 DCOMZN-CONTRATO                          PIC  X(019).
           10 DCOMZN-CANAL                             PIC  X(004).
           10 DCOMZN-IND-LIQ-CANC                      PIC  X(001).
           10 DCOMZN-PROTOCOLO                         PIC  X(050).
