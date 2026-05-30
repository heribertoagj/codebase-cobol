      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENM - AREA DO MODULO BVVEA513            *
      *  DESCRICAO.....: OBTER A AGENDA DE RECEBIVEIS DO CLIENTE       *
      *  TAMANHO.......: 00090 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  BANDEIRA......: BANDEIRA CORPORATIVA BCPP VISAO 2             *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO DE CONSISTENCIA                     *
      *                  12 - AGENDA NAO ENCONTRADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  VLR SALDO.....: VALOR DISPONIVEL DA BANDEIRA                  *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENM-COMMAREA.
           05 BVVENM-AREA-ENTRADA.
              10 BVVENM-E-AGENCIA                      PIC  9(005).
              10 BVVENM-E-CONTA                        PIC  9(013).
              10 BVVENM-E-BAND                         PIC  9(003).

           05 BVVENM-AREA-SAIDA.
              10 BVVENM-S-COD-RETORNO                  PIC  9(002).
              10 BVVENM-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENM-S-VLR-DISP                     PIC  9(015)V9(2).
