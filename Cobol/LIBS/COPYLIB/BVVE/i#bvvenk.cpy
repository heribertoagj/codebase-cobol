      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK.: I#BVVENK - AREA DO MODULO BVVEA511             *
      *  DESCRICAO....: VERIFICAR SE SOLICITACAO JA FOI PROCESSADA     *
      *  TAMANHO......: 00182 BYTES                                    *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO....: PROTOCOLO P/ RASTREIO DA PESQUISA, COMPOSICAO: *
      *                 CCUSTO X(4) + TIMESTAMP X(26) + AGENCIA 9(5) + *
      *                 CONTA 9(13) + FILLER (2)                       *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO..: 00 - PROCESSAMENTO OK                          *
      *                 01 - EM PROCESSAMENTO                          *
      *                 08 - ERRO CONSISTENCIA                         *
      *                 16 - ERRO DE DB2                               *
      *                                                                *
      *  MSG-RETORNO..: MENSGEM DE RETORNO                             *
      *  PROTOCOLO....: PROTOCOLO P/ RASTREIO DA PESQUISA              *
      *  SITUACAO.....: DESCRITIVO DA SITUACAO DO PROTOCOLO            *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENK-COMMAREA.
           05 BVVENK-AREA-ENTRADA.
              10 BVVENK-E-PROTOCOLO                    PIC  X(050).

           05 BVVENK-AREA-SAIDA.
              10 BVVENK-S-COD-RETORNO                  PIC  9(002).
              10 BVVENK-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENK-S-PROTOCOLO                    PIC  X(050).
              10 BVVENK-S-SITUACAO                     PIC  X(030).
