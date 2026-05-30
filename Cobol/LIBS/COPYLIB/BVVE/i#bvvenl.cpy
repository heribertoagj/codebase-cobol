      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENL - AREA DO MODULO BVVEA512            *
      *  DESCRICAO.....: ENVIAR CONSULTA AGENDA DE RECEBIVEIS À CIP    *
      *  TAMANHO.......: 158 BYTES                                     *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CCUSTO........: CENTRO DE CUSTO                               *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  10 - GRADE FECHADA                            *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  PROTOCOLO.....: PROTOCOLO PARA RASTREIO DE SOLICITACAO        *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *  TMP VALIDADE..: TEMPO DE VALIDADE DO PROTOCOLO  (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENL-COMMAREA.
           05 BVVENL-AREA-ENTRADA.
              10 BVVENL-E-AGENCIA                      PIC  9(005).
              10 BVVENL-E-CONTA                        PIC  9(013).
              10 BVVENL-E-CCUSTO                       PIC  X(004).

           05 BVVENL-AREA-SAIDA.
              10 BVVENL-S-COD-RETORNO                  PIC  9(002).
              10 BVVENL-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENL-S-AGENCIA                      PIC  9(005).
              10 BVVENL-S-CONTA                        PIC  9(013).
              10 BVVENL-S-PROTOCOLO                    PIC  X(050).
              10 BVVENL-S-TMP-ESPERA                   PIC  X(008).
              10 BVVENL-S-TMP-VALIDADE                 PIC  X(008).
