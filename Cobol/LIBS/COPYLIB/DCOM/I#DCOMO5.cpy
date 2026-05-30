      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0O5 - DB2PRD.TCOMPV_AGNDA_ANTEC      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7710 - SELECT         - DCOM7711 - FETCH              *
      *    - DCOM7712 - INSERT         - DCOM7713 - UPDATE             *
      *    - DCOM7714 - DELETE                                         *
      *================================================================*

       01  O5-ARGUMENTOS-ENTRADA.

       05  O5-COLUNAS-DA-TABELA.
           10  O5-CBCO                 PIC S9(003)      COMP-3.
           10  O5-CAG-BCRIA            PIC S9(005)      COMP-3.
           10  O5-CCTA-BCRIA-CLI       PIC S9(013)      COMP-3.
           10  O5-CIDTFD-PRODT-CATAO   PIC S9(005)      COMP-3.
           10  O5-HCOMPV-AGNDA-ANTEC   PIC  X(026).
           10  O5-CIDTFD-TPO-MANUT     PIC  X(001).
           10  O5-CIDTFD-TPO-AGNDA     PIC  X(001).
           10  O5-RJUSTF-AGNDA-CATAO   PIC  X(040).
           10  O5-PAGNDA-ANTEC-CATAO   PIC S9(003)V9(2) COMP-3.
           10  O5-VAGNDA-ANTEC-CATAO   PIC S9(015)V9(2) COMP-3.
           10  O5-CORIGE-ANTEC-PGMD    PIC S9(001)      COMP-3.
           10  O5-HINCL-REG            PIC  X(026).
           10  O5-HATULZ               PIC  X(026).
           10  O5-CFUNC-BDSCO          PIC S9(009)      COMP-3.
           10  O5-CTERM                PIC  X(008).
           10  O5-PTX-JURO-MES         PIC S9(003)V9(5) COMP-3.
           10  O5-VCUSTO-DESC-MES      PIC S9(008)V9(7) COMP-3.
           10  O5-VCUSTO-DESC-ANO      PIC S9(008)V9(7) COMP-3.
           10  O5-VMIN-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  O5-VMAX-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  O5-VIOF-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           10  O5-PALIQT-IOF-DESC      PIC S9(003)V9(5) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O5-QTDE-A-DESPREZAR         PIC  9(005).
       05  O5-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O5-INSTRUCAO                PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O5-RETORNO.
       05  O5-COD-RETORNO              PIC  9(004).
       05  O5-MENSAGEM                 PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O5-PROXIMO-RESTART          PIC  9(005).
       05  O5-QTDE-RETORNADA           PIC  9(003).
       05  O5-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O5-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TO5.
           10  O5RT-CBCO               PIC S9(003)      COMP-3.
           10  O5RT-CAG-BCRIA          PIC S9(005)      COMP-3.
           10  O5RT-CCTA-BCRIA-CLI     PIC S9(013)      COMP-3.
           10  O5RT-CIDTFD-PRODT-CATAO PIC S9(005)      COMP-3.
           10  O5RT-HCOMPV-AGNDA-ANTEC PIC  X(026).
           10  O5RT-CIDTFD-TPO-MANUT   PIC  X(001).
           10  O5RT-CIDTFD-TPO-AGNDA   PIC  X(001).
           10  O5RT-RJUSTF-AGNDA-CATAO PIC  X(040).
           10  O5RT-PAGNDA-ANTEC-CATAO PIC S9(003)V9(2) COMP-3.
           10  O5RT-VAGNDA-ANTEC-CATAO PIC S9(015)V9(2) COMP-3.
           10  O5RT-CORIGE-ANTEC-PGMD  PIC S9(001)      COMP-3.
           10  O5RT-HINCL-REG          PIC  X(026).
           10  O5RT-HATULZ             PIC  X(026).
           10  O5RT-CFUNC-BDSCO        PIC S9(009)      COMP-3.
           10  O5RT-CTERM              PIC  X(008).
           10  O5RT-PTX-JURO-MES       PIC S9(003)V9(5) COMP-3.
           10  O5RT-VCUSTO-DESC-MES    PIC S9(008)V9(7) COMP-3.
           10  O5RT-VCUSTO-DESC-ANO    PIC S9(008)V9(7) COMP-3.
           10  O5RT-VMIN-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  O5RT-VMAX-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  O5RT-VIOF-OPER-DESC     PIC S9(015)V9(2) COMP-3.
           10  O5RT-PALIQT-IOF-DESC    PIC S9(003)V9(5) COMP-3.

