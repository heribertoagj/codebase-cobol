      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENF - AREA DO MODULO BVVEA506            *
      *  DESCRICAO.....: SOLICITACAO DE CANCELAMENTO DE CESSAO         *
      *  TAMANHO.......: 00152 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CONTRATO......: NUMERO DO CONTRATO                            *
      *  CANAL.........: CANAL SOLICITANTE                             *
      *  IND-LIQ-CANC..: LIQUIDACAO "L" OU CANCELAMENTO "C"            *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  10 - GRADE FECHADA                            *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM RETORNO                              *
      *  PROTOCOLO.....: GERADO PARA CONTROLE DE PROCESSAMENTO         *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENF-COMMAREA.
           05 BVVENF-AREA-ENTRADA.
              10 BVVENF-E-AGENCIA                      PIC  9(005).
              10 BVVENF-E-CONTA                        PIC  9(013).
              10 BVVENF-E-CONTRATO                     PIC  X(019).
              10 BVVENF-E-CANAL                        PIC  X(004).
              10 BVVENF-E-IND-LIQ-CANC                 PIC  X(001).

           05 BVVENF-AREA-SAIDA.
              10 BVVENF-S-COD-RETORNO                  PIC  9(002).
              10 BVVENF-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENF-S-TMP-ESPERA                   PIC  X(008).
              10 BVVENF-S-PROTOCOLO                    PIC  X(050).
