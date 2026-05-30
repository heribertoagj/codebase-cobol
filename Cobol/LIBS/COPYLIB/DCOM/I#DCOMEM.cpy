      *---------------------------------------------------------------*
      *         EMPF - SISTEMA DE EMPRESTIMOS E FINANCIAMENTOS        *
      *                                                               *
      *                S I N T E T I C O                              *
      *                =================                              *
      *                                                               *
      *   INCLUDE          : I#DCOMEM                                 *
      *   TAMANHO          : 135                                      *
      *   RESPONSAVEL      : FRUTEIRO/LEANDRO                         *
      *   DATA DE CRIACAO  : 05.05.2005                               *
      *---------------------------------------------------------------*

       01  DCOMEM-REGTO.
           03  DCOMEM-DATA-BASE            PIC  X(10).
           03  DCOMEM-CJUNC-DEPDC          PIC  9(05).
           03  DCOMEM-PAB                  PIC  9(03).
           03  DCOMEM-SEGMENTO             PIC  9(03).
           03  DCOMEM-CCART                PIC  X(03).
           03  DCOMEM-QCONTR-DIA           PIC  9(15).
           03  DCOMEM-VALOR-DIA            PIC  9(15)V9(2).
           03  DCOMEM-QCONTR-ESTOQUE       PIC  9(15).
           03  DCOMEM-VALOR-ESTOQUE        PIC  9(15)V9(2).
           03  DCOMEM-QTIT-DIA             PIC  9(15).
           03  DCOMEM-QTIT-ESTOQUE         PIC  9(15).
           03  DCOMEM-VALOR-VENCD-ESTOQUE  PIC  9(15)V9(2).

      *---------------------------------------------------------------*
      *     DCOMEM-REGTO                          1   135  A          *
      *     DCOMEM-DATA-BASE                      1    10  A          *
      *     DCOMEM-CJUNC-DEPDC                   11     5  N          *
      *     DCOMEM-PAB                           16     3  N          *
      *     DCOMEM-SEGMENTO                      19     3  N          *
      *     DCOMEM-CCART                         22     3  A          *
      *     DCOMEM-QCONTR-DIA                    25    15  N          *
      *     DCOMEM-VALOR-DIA                     40    17  N          *
      *     DCOMEM-QCONTR-ESTOQUE                57    15  N          *
      *     DCOMEM-VALOR-ESTOQUE                 72    17  N          *
      *     DCOMEM-QTIT-DIA                      89    15  N          *
      *     DCOMEM-QTIT-ESTOQUE                 104    15  N          *
      *     DCOMEM-VALOR-VENCD-ESTOQU           119    17  N          *
      *---------------------------------------------------------------*
