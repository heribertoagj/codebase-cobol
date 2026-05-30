      *---------------------------------------------------------------*
      *                      SONDA IT                                 *
      *---------------------------------------------------------------*
      *    NOME DO BOOK: CMTFWES1                                     *
      *    ANALISTA    : AIRAM JAIR TEIXEIRA                          *
      *    DATA        : 04/2012                                      *
      *    COMENTARIO  : AREA DE COMUNICACAO COM O PROGRAMA CMTF2EST  *
      *---------------------------------------------------------------*

       01  CMTFWES1.
           03  CMTFWES1-PROGRAMA            PIC  X(008).

           03  CMTFWES1-QTD-ARQUIVOS        PIC  9(003) COMP-3 VALUE 0.
           03  CMTFWES1-QTD-TABELAS         PIC  9(003) COMP-3 VALUE 0.
           03  CMTFWES1-QTD-COMENTARIOS     PIC  9(003) COMP-3 VALUE 0.

           03  CMTFWES1-DADOS-ARQUIVOS      OCCURS  30  TIMES.
               05  CMTFWES1-I-O             PIC  X(001).
      *            I - INPUT
      *            O - OUTPUT
               05  CMTFWES1-DDNAME          PIC  X(008).
               05  CMTFWES1-DESCARQ         PIC  X(027).
               05  CMTFWES1-QTDEARQ         PIC  9(012).

           03  CMTFWES1-DADOS-TABELAS       OCCURS  30  TIMES.
               05  CMTFWES1-COMANDO         PIC  X(001).
      *            S - SELECT
      *            I - INSERT
      *            D - DELETE
      *            U - UPDATE
               05  CMTFWES1-TABLENAME       PIC  X(008).
               05  CMTFWES1-DESCTAB         PIC  X(027).
               05  CMTFWES1-QTDETAB         PIC  9(012).

           03  CMTFWES1-DADOS-PARM.
               05  CMTFWES1-PARM            PIC  X(057).

           03  CMTFWES1-DADOS-COMENTARIO    OCCURS  10  TIMES.
               05  CMTFWES1-COMENTARIO      PIC  X(059).

           03  CMTFWES1-DADOS-RESTART.
               05  CMTFWES1-RESTART         PIC  X(048).

           03  CMTFWES1-DADOS-COMPILACAO.
               05  CMTFWES1-DATA-COMP       PIC  X(010).
               05  CMTFWES1-HORA-COMP       PIC  X(008).

           03  CMTFWES1-DADOS-MOVIMENTO.
               05  CMTFWES1-DATA-MOVIMENTO  PIC  X(010).
