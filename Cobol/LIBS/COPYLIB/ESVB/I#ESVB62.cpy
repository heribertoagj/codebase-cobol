      ****************************************************************
      * NOME BOOK : I#ESVB62                                         *
      * DESCRICAO : COPY  PARA PROGRAMA ESVB1062                     *
      *             FIXO - 350 POS                                   *
      * DATA      : 03/10/2008                                       *
      * AUTOR     : CARLOS R MORAES                                  *
      * EMPRESA   : GFT                                              *
      * GRUPO     : TI MELHORIAS                                     *
      * COMPONENTE: SOLICITACAO DE EXECUCAO DE PROCESSOS             *
      *                                                              *
      *--------------------------------------------------------------*
      *  DATA      AUTOR     MODIFICACAO                             *
      * ---------- -------  -----------------------------------------*
      * 99/99/9999 XXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX              *
      *--------------------------------------------------------------*
      *                                                              *
      ****************************************************************
      *
           05  ESVBEL62-HEADER.
               10  ESVBEL62-COD-LAYOUT       PIC X(08) VALUE 'ESVBEL62'.
               10  ESVBEL62-TAM-LAYOUT       PIC 9(05) VALUE 350.
      *
           05  ESVBEL62-REGISTRO.
               10  ESVBEL62-BLOCO-ENTRADA.
                   15  ESVBEL62-E-CODVERSAO        PIC 9(002).
                   15  ESVBEL62-E-PGM-CHAMADOR     PIC X(008).
                   15  ESVBEL62-E-CFUNCL-PROCS     PIC 9(008).
                   15  ESVBEL62-E-CFUNCL-PROCS-R   REDEFINES
                       ESVBEL62-E-CFUNCL-PROCS     PIC X(008).
                   15  ESVBEL62-E-AGENDA-IMEDIATA  PIC X(001).
                   15  ESVBEL62-E-DATA-AGENDA      PIC X(010).
                   15  ESVBEL62-E-HORA-AGENDA      PIC X(008).
                   15  ESVBEL62-E-PARAMETRO        PIC X(300).
      *
      *----------------------------------------------------------------*
