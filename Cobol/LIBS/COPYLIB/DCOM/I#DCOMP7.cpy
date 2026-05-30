      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0P7 - DB2PRD.TCONTR_OPCAO_PGTO                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7740 - SELECT         - DCOM7741 - FETCH             *
      *    - DCOM7742 - INSERT         - DCOM7743 - UPDATE            *
      *    - DCOM7744 - DELETE                                        *
      *===============================================================*

       01  P7-ARGUMENTOS-ENTRADA.
           05 P7-COLUNAS-DA-TABELA.
              10 P7-CCONTR-LIM-DESC    PIC S9(009)V COMP-3.
              10 P7-CVRSAO-CONTR-LIM   PIC S9(003)V COMP-3.
              10 P7-CINDCD-FORMA-LIQDC PIC  X(001).
              10 P7-CINDCD-UTILZ-LIM   PIC  X(001).
              10 P7-CINDCD-DEB-PCIAL   PIC  X(001).
              10 P7-CINDCD-TENTV-DEB   PIC  X(001).
              10 P7-HULT-ATULZ         PIC  X(026).
              10 P7-CFUNC-BDSCO        PIC S9(009)V USAGE COMP-3.
              10 P7-CTERM              PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 P7-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 P7-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  P7-RETORNO.
           05 P7-COD-RETORNO           PIC  9(004).
           05 P7-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 P7-PROXIMO-RESTART       PIC  9(005).
           05 P7-QTDE-RETORNADA        PIC  9(003).
           05 P7-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 P7-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TP7.
              10 P7RT-CCONTR-LIM-DESC  PIC S9(009)V COMP-3.
              10 P7RT-CVRSAO-CONTR-LIM PIC S9(003)V COMP-3.
              10 P7RT-CINDCD-FORMA-LIQDC
                                       PIC  X(001).
              10 P7RT-CINDCD-UTILZ-LIM PIC  X(001).
              10 P7RT-CINDCD-DEB-PCIAL PIC  X(001).
              10 P7RT-CINDCD-TENTV-DEB PIC  X(001).
              10 P7RT-HULT-ATULZ       PIC  X(026).
              10 P7RT-CFUNC-BDSCO      PIC S9(009)V COMP-3.
              10 P7RT-CTERM            PIC  X(008).
