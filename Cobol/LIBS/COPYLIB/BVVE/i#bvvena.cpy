      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK....: I#BVVENA - AREA DO MODULO BVVEA501          *
      *  DESCRICAO.......: VERIFICAR SE SOLICITACAO JA FOI PROCESSADA  *
      *  TAMANHO.........: 182 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO...: PROTOCOLO P/ RASTREIO DA PESQUISA, COMPOSICAO:  *
      *                CANAL X(4) + TIMESTAMP X(26) + AGENCIA 9(5) +   *
      *                CONTA 9(13) + FILLER X(3)                       *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO : 00 - PROCESSAMENTO OK                           *
      *                01 - EM PROCESSAMENTO                           *
      *                08 - ERRO CONSISTENCIA                          *
      *                16 - ERRO DE DB2                                *
      *                                                                *
      *  MSG-RETORNO.: MENSAGEM DE RETORNO                             *
      *  PROTOCOLO...: PROTOCOLO P/ RASTREIO DA PESQUISA               *
      *  SITUACAO....: DESCRITIVO DA SITUACAO DO PROTOCOLO             *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENA-COMMAREA.
           05 BVVENA-AREA-ENTRADA.
              10 BVVENA-E-PROTOCOLO                    PIC  X(050).

           05 BVVENA-AREA-SAIDA.
              10 BVVENA-S-COD-RETORNO                  PIC  9(002).
              10 BVVENA-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENA-S-PROTOCOLO                    PIC  X(050).
              10 BVVENA-S-SITUACAO                     PIC  X(030).
