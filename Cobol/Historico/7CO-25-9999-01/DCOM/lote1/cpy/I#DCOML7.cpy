      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0L7 - DB2PRD.TMSGEM_BACEN_DESC                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7565 - SELECT         - DCOM7566 - FETCH             *
      *    - DCOM7567 - INSERT         - DCOM7568 - UPDATE            *
      *    - DCOM7569 - DELETE                                        *
      *===============================================================*

       01  L7-ARGUMENTOS-ENTRADA.
           05 L7-COLUNAS-DA-TABELA.
              10 L7-DANO-OPER-DESC     PIC S9(004)V COMP-3.
              10 L7-NSEQ-OPER-DESC     PIC S9(009)V COMP-3.
              10 L7-HINCL-REG          PIC  X(026).
              10 L7-CMSGEM-RECOR       PIC  X(009).
              10 L7-CISPB-EMISR-MSGEM  PIC  X(008).
              10 L7-CISPB-DSTNA-MSGEM  PIC  X(008).
              10 L7-CCTRL-RQUIS-FINCR  PIC  X(020).
              10 L7-CUNIC-MSGEM        PIC  X(023).
              10 L7-CSIT-DESC-COML     PIC S9(003)V COMP-3.
              10 L7-HULT-ATULZ         PIC  X(026).
              10 L7-WCONTD-MSGEM-DESC.
                 15 L7-WCONTD-MSGEM-DESC-LEN
                                       PIC S9(004) COMP.
                 15 L7-WCONTD-MSGEM-DESC-TEXT
                                       PIC X(4000).
              10 L7-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 L7-CTERM              PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
           05 L7-QTDE-A-DESPREZAR      PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
           05 L7-INSTRUCAO             PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************

       01  L7-RETORNO.
           05 L7-COD-RETORNO           PIC  9(004).
           05 L7-MENSAGEM              PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05 L7-PROXIMO-RESTART       PIC  9(005).
           05 L7-QTDE-RETORNADA        PIC  9(003).
           05 L7-QTDE-TOTAL            PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05 L7-OCORRENCIAS    OCCURS 070 TIMES INDEXED BY IND-TL7.
              10 L7RT-DANO-OPER-DESC     PIC S9(004)V COMP-3.
              10 L7RT-NSEQ-OPER-DESC     PIC S9(009)V COMP-3.
              10 L7RT-HINCL-REG          PIC  X(026).
              10 L7RT-CMSGEM-RECOR       PIC  X(009).
              10 L7RT-CISPB-EMISR-MSGEM  PIC  X(008).
              10 L7RT-CISPB-DSTNA-MSGEM  PIC  X(008).
              10 L7RT-CCTRL-RQUIS-FINCR  PIC  X(020).
              10 L7RT-CUNIC-MSGEM        PIC  X(023).
              10 L7RT-CSIT-DESC-COML     PIC S9(003)V COMP-3.
              10 L7RT-HULT-ATULZ         PIC  X(026).
              10 L7RT-WCONTD-MSGEM-DESC.
                 15 L7RT-WCONTD-MSGEM-DESC-LEN
                                         PIC S9(004) COMP.
                 15 L7RT-WCONTD-MSGEM-DESC-TEXT
                                         PIC X(4000).
              10 L7RT-CFUNC-BDSCO        PIC S9(009)V COMP-3.
              10 L7RT-CTERM              PIC  X(008).
