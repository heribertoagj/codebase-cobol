      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENJ - AREA DO MODULO BVVEA510            *
      *  DESCRICAO.....: OBTER A AGENDA DE RECEBIVEIS DO CLIENTE POR   *
      *                  DATA E PRODUTO (ANTICA TC03 TELEBANCO)        *
      *  TAMANHO.......: 03351 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  PRODUTO.......: PRODUTO DO RECEBIVEL                          *
      *  MOEDA.........: MOEDA                                         *
      *                                                                *
      *--RESTART-------------------------------------------------------*
      *                                                                *
      *  DATA..........: DATA DO RECEBIVEL (DD.MM.AAAA)                *
      *                  MOVER BRANCOS NO PRIMEIRO ACESSO              *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  01 - RESTART                                  *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  12 - AGENDA NAO ENCONTRADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENJ-COMMAREA.
           05 BVVENJ-AREA-ENTRADA.
              10 BVVENJ-E-AGENCIA                PIC  9(005).
              10 BVVENJ-E-CONTA                  PIC  9(013).
              10 BVVENJ-E-PRODUTO                PIC  9(005).
              10 BVVENJ-E-MOEDA                  PIC  9(005).

           05 BVVENJ-AREA-RESTART.
              10 BVVENJ-R-DATA                   PIC  X(010).

           05 BVVENJ-AREA-SAIDA.
              10 BVVENJ-S-COD-RETORNO            PIC  9(002).
              10 BVVENJ-S-MSG-RETORNO            PIC  X(050).
              10 BVVENJ-S-AGENCIA                PIC  9(005).
              10 BVVENJ-S-CONTA                  PIC  9(013).
              10 BVVENJ-S-PRODUTO                PIC  9(005).
              10 BVVENJ-S-DESC-PROD              PIC  X(025).
              10 BVVENJ-S-NOME-CLIENTE           PIC  X(035).
              10 BVVENJ-S-MOEDA                  PIC  9(005).
              10 BVVENJ-S-DESC-MOEDA             PIC  X(016).
              10 BVVENJ-S-TOT-CREDITO            PIC  9(015)V99.
              10 BVVENJ-S-TOT-ANTECIP            PIC  9(015)V99.
              10 BVVENJ-S-TOT-SALD-SIN           PIC  X(001).
              10 BVVENJ-S-TOT-SALDO              PIC  9(015)V99.
              10 BVVENJ-S-QTDE-OCORR             PIC  9(005).
              10 BVVENJ-S-TABELA       OCCURS 50 TIMES.
                 15 BVVENJ-S-DATA-UR             PIC  X(010).
                 15 BVVENJ-S-CRED-UR             PIC  9(015)V99.
                 15 BVVENJ-S-ANTE-UR             PIC  9(015)V99.
                 15 BVVENJ-S-SALD-UR-SIN         PIC  X(001).
                 15 BVVENJ-S-SALD-UR             PIC  9(015)V99.
