      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENB - AREA DO MODULO BVVEA502            *
      *  DESCRICAO.....: ENVIAR CONSULTA AGENDA DE RECEBIVEIS À CIP    *
      *  TAMANHO.......: 00158 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CANAL.........: CANAL SOLICITANTE                             *
      *                  NETE (NET EMPRESA) / MOBI (MOBILE)            *
      *                  AGEN (AGENCIA)     / PROG (PROGRAMADO)        *
      *                  TELE (TELEBANCO)                              *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  10 - GRADE FECHADA                            *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  PROTOCOLO.....: PROTOCOLO P/ RASTREIO DA PESQUISA, COMPOSICAO:*
      *                  CANAL X(4) + TIMESTAMP X(26) + AGENCIA 9(5) + *
      *                  CONTA 9(13) + FILLER (2)                      *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *  TMP VALIDADE..: TEMPO DE VALIDADE DO PROTOCOLO  (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENB-COMMAREA.
           05 BVVENB-AREA-ENTRADA.
              10 BVVENB-E-AGENCIA                      PIC  9(005).
              10 BVVENB-E-CONTA                        PIC  9(013).
              10 BVVENB-E-CANAL                        PIC  X(004).

           05 BVVENB-AREA-SAIDA.
              10 BVVENB-S-COD-RETORNO                  PIC  9(002).
              10 BVVENB-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENB-S-AGENCIA                      PIC  9(005).
              10 BVVENB-S-CONTA                        PIC  9(013).
              10 BVVENB-S-PROTOCOLO                    PIC  X(050).
              10 BVVENB-S-TMP-ESPERA                   PIC  X(008).
              10 BVVENB-S-TMP-VALIDADE                 PIC  X(008).
