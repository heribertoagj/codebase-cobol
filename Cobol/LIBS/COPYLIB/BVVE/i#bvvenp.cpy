      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENP - AREA DO MODULO BVVEA516            *
      *  DESCRICAO.....: SOLICITACAO DE CANCELAMENTO DE GRAVAME        *
      *  TAMANHO.......: 00134 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  CENTRO CUSTO..: CENTO DE CUSTO ORIGEM                         *
      *  CONTRATO......: NUMERO DO CONTRATO GRAN                       *
      *  IND-LIQ-CANC..: LIQUIDACAO "L" OU CANCELAMENTO "C"            *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: CODIGO DE RETORNO                             *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  PROTOCOLO.....: PROTOCOLO PARA RASTREIO DE SOLICITACAO        *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENP-COMMAREA.
           05 BVVENP-AREA-ENTRADA.
              10 BVVENP-E-CCUSTO                       PIC  X(004).
              10 BVVENP-E-CONTRATO                     PIC  X(019).
              10 BVVENP-E-IND-LIQ-CANC                 PIC  X(001).

           05 BVVENP-AREA-SAIDA.
              10 BVVENP-S-COD-RETORNO                  PIC  9(002).
              10 BVVENP-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENP-S-PROTOCOLO                    PIC  X(050).
              10 BVVENP-S-TMP-ESPERA                   PIC  X(008).
