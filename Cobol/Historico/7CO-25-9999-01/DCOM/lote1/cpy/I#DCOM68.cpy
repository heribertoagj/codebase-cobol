      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB068 - DB2PRD.TSPROD_REGRA_OPER                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6840 - SELECT         - DCOM6841 - FETCH             *
      *    - DCOM6842 - INSERT         - DCOM6843 - UPDATE            *
      *    - DCOM6844 - DELETE                                        *
      *===============================================================*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 22/08/2012 - INCLUIR HORARIO DE REGISTRO DE NPR NO BACEN *
DTS001*===============================================================*
BSI001* ALTERACOES.........:                                          *
BSI001*    - 20/02/2014 - INCLUIR PARAMETROS PARA REGISTRO DA OPERA-  *
BSI001*                   CAO DE NPR NO BACEN.                        *
BSI001*      OBS. AS PARAMETRIZACOES DE VALOR, PRAZO E TAXA DO BACEN  *
BSI001*           ESTAO NA TABELA DCOMB0M1 - TSPROD_BACEN_REGRA.      *
BSI001*===============================================================*
BBR001* ALTERACOES.........:                                          *
BRQ001*    - 25/04/2018 - INCLUIR PARAMETRO OBRIGATORIEDADE DO CAR.   *
BRQ001*===============================================================*

       01  68-ARGUMENTOS-ENTRADA.

       05  68-COLUNAS-DA-TABELA.
           10  68-CPRODT                   PIC S9(3)V COMP-3.
           10  68-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  68-HINIC-REGRA-OPER         PIC X(26).
           10  68-HFIM-REGRA-OPER          PIC X(26).
           10  68-CINDCD-CALC-JURO         PIC X(1).
           10  68-CINDCD-PGDOR-JURO        PIC X(1).
           10  68-CINDCD-TX-REDE           PIC X(1).
           10  68-CINDCD-INCID-IOF         PIC X(1).
           10  68-CINDCD-DESC-SACDO        PIC X(1).
           10  68-CINDCD-NEGOC-DIFER       PIC X(1).
           10  68-VMIN-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  68-VMAX-OPER-DESC           PIC S9(15)V9(2) COMP-3.
           10  68-CREGRA-MORA-LEGAL        PIC S9(3)V COMP-3.
           10  68-CREGRA-GERC-CONS         PIC S9(3)V COMP-3.
           10  68-CREGRA-GERC-MORA         PIC S9(3)V COMP-3.
           10  68-CREGRA-GERC-CRATS        PIC S9(3)V COMP-3.
           10  68-HULT-ATULZ               PIC X(26).
           10  68-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  68-CTERM                    PIC X(8).
JU         10  68-CINDCD-PROTE-TITLO       PIC X(01).
JU         10  68-QDIA-PROTE-TITLO         PIC 9(03).
JU         10  68-CINDCD-DIA-PROTE         PIC X(02).
DTS001     10  68-HINIC-REG-BACEN          PIC X(08).
DTS001     10  68-HFIM-REG-BACEN           PIC X(08).
BSI001     10  68-CREG-OPER-BACEN          PIC X(01).
BRQ001     10  68-CINDCD-REG-AMBTL         PIC X(01).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  68-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  68-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  68-RETORNO.
       05  68-COD-RETORNO                         PIC 9(04).
       05  68-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  68-PROXIMO-RESTART                     PIC 9(05).
       05  68-QTDE-RETORNADA                      PIC 9(03).
       05  68-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  68-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T68.
           10  68RT-CPRODT                 PIC S9(3)V COMP-3.
           10  68RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  68RT-HINIC-REGRA-OPER       PIC X(26).
           10  68RT-HFIM-REGRA-OPER        PIC X(26).
           10  68RT-CINDCD-CALC-JURO       PIC X(1).
           10  68RT-CINDCD-PGDOR-JURO      PIC X(1).
           10  68RT-CINDCD-TX-REDE         PIC X(1).
           10  68RT-CINDCD-INCID-IOF       PIC X(1).
           10  68RT-CINDCD-DESC-SACDO      PIC X(1).
           10  68RT-CINDCD-NEGOC-DIFER     PIC X(1).
           10  68RT-VMIN-OPER-DESC         PIC S9(15)V9(2) COMP-3.
           10  68RT-VMAX-OPER-DESC         PIC S9(15)V9(2) COMP-3.
           10  68RT-CREGRA-MORA-LEGAL      PIC S9(3)V COMP-3.
           10  68RT-CREGRA-GERC-CONS       PIC S9(3)V COMP-3.
           10  68RT-CREGRA-GERC-MORA       PIC S9(3)V COMP-3.
           10  68RT-CREGRA-GERC-CRATS      PIC S9(3)V COMP-3.
           10  68RT-HULT-ATULZ             PIC X(26).
           10  68RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  68RT-CTERM                  PIC X(8).
JU         10  68RT-CINDCD-PROTE-TITLO     PIC X(01).
JU         10  68RT-QDIA-PROTE-TITLO       PIC 9(03).
JU         10  68RT-CINDCD-DIA-PROTE       PIC X(02).
DTS001     10  68RT-HINIC-REG-BACEN        PIC X(08).
DTS001     10  68RT-HFIM-REG-BACEN         PIC X(08).
BSI001     10  68RT-CREG-OPER-BACEN        PIC X(01).
BRQ001     10  68RT-CINDCD-REG-AMBTL       PIC X(01).
