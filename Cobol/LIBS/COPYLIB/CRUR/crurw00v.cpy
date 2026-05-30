      ******************************************************************
      *    NOME BOOK : CRURW00V                                        *
      *    DESCRICAO : BOOK DE INTERFACE                               *
      *    DATA      : 18/03/2013                                      *
      *    AUTOR     : DAVI RODRIGUES                                  *
      *    EMPRESA   : CAPGEMINI                                       *
      *    GRUPO     : CAPGEMINI                                       *
      *    TAMANHO   : 1024 BYTES                                      *
      *                                                                *
      ******************************************************************
      *----------------------------------------------------------------*
      *    ALTERACOES..:                                               *
      *                                                                *
      * DATA       NOME        DESCRICAO                      TAG      *
      * ========== =========== ============================== =========*
      * 14/08/2015 LUCAS       INCLUSAO DE CAMPOS             LCB      *
      *            BARRETO                                             *
      *----------------------------------------------------------------*
TCS001* 09/02/2022 FABIO       INCLUSAO DO CAMPO DE 'FATOR'.  TCS001   *
TCS001*            GRIFONI     --> CRURW00V-PFIXO-PARM-RURAL           *
      *----------------------------------------------------------------*
      ******************************************************************
      *
           05 CRURW00V-REGISTROS.
             10 CRURW00V-CEMPTO-RURAL       PIC  9(006).
             10 CRURW00V-CPROG-RURAL        PIC  9(009).
             10 CRURW00V-PTX-EFETV-JURO     PIC  9(07)V9(4).
             10 CRURW00V-PTX-AM             PIC  9(07)V9(4).
             10 CRURW00V-CPORTE-BNEFC-RURAL PIC  9(003).
             10 CRURW00V-CMODLD-INDEX       PIC  9(002).
             10 CRURW00V-CINDCD-ECONM-NUMRC PIC  9(005).
             10 CRURW00V-PAPLIC-INDCD-ECONM PIC  9(003)V9(06).
             10 CRURW00V-QDIA-BASE-CALC     PIC  9(003).
             10 CRURW00V-CORIGE-REC-RURAL   PIC  9(003).
LCB          10 CRURW00V-CTPO-REC-FINCR     PIC  9(001).
LCB          10 CRURW00V-CREC-FINCR-FINAN   PIC  9(008).
LCB          10 CRURW00V-CREC-BCO-CNTRL     PIC  9(005).
             10 CRURW00V-CSUB-PROG-PRODT    PIC  9(008).
             10 CRURW00V-CSUB-PROG-VINC     PIC  9(005).
             10 CRURW00V-CTPO-CORRECAO      PIC  X(001).
             10 CRURW00V-CD-INDEX-ECON      PIC  X(005).
BR118        10 CRURW00V-PTX-EFETV-JURO-S   PIC  X(001).
TCS001       10 CRURW00V-PFIXO-PARM-RURAL   PIC  9(003)V9(2).
TCS001       10 FILLER                      PIC  X(923).
