      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENI - AREA DO MODULO BVVEA509            *
      *  DESCRICAO.....: OBTER A AGENDA CONSOLIDADA DO CLIENTE POR     *
      *                  PRODUTO (ANTIGA TC02 TELEBANCO)               *
      *  TAMANHO.......: 00692 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  PRODUTO.......: PRODUTO DO RECEBIVEL                          *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  12 - AGENDA NAO ENCONTRADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENI-COMMAREA.
           05 BVVENI-AREA-ENTRADA.
              10 BVVENI-E-AGENCIA                PIC  9(005).
              10 BVVENI-E-CONTA                  PIC  9(013).
              10 BVVENI-E-PRODUTO                PIC  9(005).

           05 BVVENI-AREA-SAIDA.
              10 BVVENI-S-COD-RETORNO            PIC  9(002).
              10 BVVENI-S-MSG-RETORNO            PIC  X(050).
              10 BVVENI-S-AGENCIA                PIC  9(005).
              10 BVVENI-S-CONTA                  PIC  9(013).
              10 BVVENI-S-PRODUTO                PIC  9(005).
              10 BVVENI-S-DESC-PROD              PIC  X(025).
              10 BVVENI-S-PROGRAMADO             PIC  X(001).
              10 BVVENI-S-VLR-CRED-SIN           PIC  X(001).
              10 BVVENI-S-VLR-CREDITO            PIC  9(015)V99.
              10 BVVENI-S-VLR-ANTE-SIN           PIC  X(001).
              10 BVVENI-S-VLR-ANTECIP            PIC  9(015)V99.
              10 BVVENI-S-VLR-DISP-SIN           PIC  X(001).
              10 BVVENI-S-VLR-DISPONIVEL         PIC  9(015)V99.
              10 BVVENI-S-PORCENTO               PIC  9(003).
              10 BVVENI-S-MEDIA-CRED-PERC        PIC  9(015)V99.
              10 BVVENI-S-HISTORICO    OCCURS 6 TIMES.
                 15 BVVENI-S-HIST-MES            PIC  X(010).
                 15 BVVENI-S-HIST-CRED           PIC  9(015)V99.
                 15 BVVENI-S-HIST-ANTE           PIC  9(015)V99.
                 15 BVVENI-S-HIST-DEBI           PIC  9(015)V99.
              10 BVVENI-S-MEDIA-CRED             PIC  9(015)V99.
              10 BVVENI-S-MEDIA-ANTE             PIC  9(015)V99.
              10 BVVENI-S-MEDIA-DEBI             PIC  9(015)V99.
              10 BVVENI-S-VLR-LIMITE             PIC  9(015)V99.
              10 BVVENI-S-PERC-LIMITE            PIC  9(003)V99.
              10 BVVENI-S-ACORDO                 PIC  X(001).
              10 BVVENI-S-TAXA                   PIC  9(003)V99.
              10 BVVENI-S-CFUNC-BDSCO            PIC  9(009).
              10 BVVENI-S-IFUNC-BDSCO            PIC  X(040).
