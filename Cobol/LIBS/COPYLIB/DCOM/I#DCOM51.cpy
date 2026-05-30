      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB051 - DB2PRD.TREGRA_MORA_GERC                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6755 - SELECT         - DCOM6756 - FETCH             *
      *    - DCOM6757 - INSERT         - DCOM6758 - UPDATE            *
      *    - DCOM6759 - DELETE                                        *
      *===============================================================*
BSI001*                 ALTERACAO - BSI TECNOLOGIA                    *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:  LUCIANDRA SILVEIRA  -  BSI TECNOLOGIA         *
BSI001*  DATA........:  22/08/2017                                    *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:  INCLUIR CODIGO DE IDENTIFICACAO PARA ENCARGOS *
BSI001*                 MORATORIOS.                                   *
BSI001*                 BSI 17/0097 - ENCARGOS MORATORIOS - FASE 3    *
BSI001*===============================================================*

       01  51-ARGUMENTOS-ENTRADA.

       05  51-COLUNAS-DA-TABELA.
           10  51-CREGRA-MORA-GERC         PIC S9(3)V COMP-3.
           10  51-IREGRA-MORA-GERC         PIC X(40).
           10  51-TINIC-VCTO-PCELA         PIC S9(5)V COMP-3.
           10  51-TFNAL-VCTO-PCELA         PIC S9(5)V COMP-3.
           10  51-CINDCD-CALC-MORA         PIC S9(1)V COMP-3.
           10  51-CIDTFD-CORRC-DIVDA       PIC S9(5)V COMP-3.
           10  51-CIDTFD-JURO-MORA         PIC S9(5)V COMP-3.
           10  51-CINDCD-RECAL-VENCD       PIC X(1).
           10  51-HULT-ATULZ               PIC X(26).
           10  51-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  51-CTERM                    PIC X(8).
BSI001     10  51-CIDTFD-JURO-REMUN        PIC S9(5)V     COMP-3.
BSI001     10  51-PTX-JURO-REMUN           PIC S9(3)V9(3) COMP-3.
BSI001     10  51-CIDTFD-MORA-VENCD        PIC S9(5)V     COMP-3.
BSI001     10  51-PTX-MORA-VENCD           PIC S9(3)V9(3) COMP-3.
BSI001     10  51-CIDTFD-MULTA-VENCD       PIC S9(5)V     COMP-3.
BSI001     10  51-PTX-MULTA-VENCD          PIC S9(3)V9(3) COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  51-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  51-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  51-RETORNO.
       05  51-COD-RETORNO                         PIC 9(04).
       05  51-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  51-PROXIMO-RESTART                     PIC 9(05).
       05  51-QTDE-RETORNADA                      PIC 9(03).
       05  51-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  51-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T51.
           10  51RT-CREGRA-MORA-GERC       PIC S9(3)V COMP-3.
           10  51RT-IREGRA-MORA-GERC       PIC X(40).
           10  51RT-TINIC-VCTO-PCELA       PIC S9(5)V COMP-3.
           10  51RT-TFNAL-VCTO-PCELA       PIC S9(5)V COMP-3.
           10  51RT-CINDCD-CALC-MORA       PIC S9(1)V COMP-3.
           10  51RT-CIDTFD-CORRC-DIVDA     PIC S9(5)V COMP-3.
           10  51RT-CIDTFD-JURO-MORA       PIC S9(5)V COMP-3.
           10  51RT-CINDCD-RECAL-VENCD     PIC X(1).
           10  51RT-HULT-ATULZ             PIC X(26).
           10  51RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  51RT-CTERM                  PIC X(8).
BSI001     10  51RT-CIDTFD-JURO-REMUN      PIC S9(5)V     COMP-3.
BSI001     10  51RT-PTX-JURO-REMUN         PIC S9(3)V9(3) COMP-3.
BSI001     10  51RT-CIDTFD-MORA-VENCD      PIC S9(5)V     COMP-3.
BSI001     10  51RT-PTX-MORA-VENCD         PIC S9(3)V9(3) COMP-3.
BSI001     10  51RT-CIDTFD-MULTA-VENCD     PIC S9(5)V     COMP-3.
BSI001     10  51RT-PTX-MULTA-VENCD        PIC S9(3)V9(3) COMP-3.
