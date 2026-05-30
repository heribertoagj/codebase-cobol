      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0J3 - DB2PRD.TPOSIC_CHEQ_ATIVO      *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      *****************************************************************
       01  CF-ARGUMENTOS-ENTRADA.

        05 CF-COLUNAS-DA-TABELA.
           10 CF-CBCO                   PIC S9(003)V     USAGE COMP-3.
           10 CF-CAG-BCRIA              PIC S9(005)V     USAGE COMP-3.
           10 CF-CCTA-BCRIA-CLI         PIC S9(013)V     USAGE COMP-3.
           10 CF-CPRODT                 PIC S9(003)V     USAGE COMP-3.
           10 CF-CSPROD-DESC-COML       PIC S9(003)V     USAGE COMP-3.
           10 CF-DANO-OPER-DESC         PIC S9(004)V     USAGE COMP-3.
           10 CF-NSEQ-OPER-DESC         PIC S9(009)V     USAGE COMP-3.
           10 CF-NPCELA-DESC-COML       PIC S9(005)V     USAGE COMP-3.
           10 CF-CSIT-DESC-COML         PIC S9(003)V     USAGE COMP-3.
           10 CF-DVCTO-PCELA-DESC       PIC  X(010).
           10 CF-VPRINC-PCELA-DESC      PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-VJURO-PCELA-DESC       PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-VIOF-PCELA-DESC        PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-VSDO-DVDOR-PRINC       PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-VJURO-MORA-GERC        PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-VIOF-PCELA-VENCD       PIC S9(015)V9(2) USAGE COMP-3.
           10 CF-QTITLO-CHEQ-PCELA      PIC S9(009)V     USAGE COMP-3.
           10 CF-TOPER-DESC-APURA       PIC S9(003)V     USAGE COMP-3.
           10 CF-FILLER                 PIC  X(100).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
        05 CF-QTDE-A-DESPREZAR          PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
        05 CF-INSTRUCAO                 PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  CF-RETORNO.
        05 CF-COD-RETORNO               PIC  9(004).
        05 CF-MENSAGEM                  PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
        05 CF-PROXIMO-RESTART           PIC  9(005).
        05 CF-QTDE-RETORNADA            PIC  9(003).
        05 CF-QTDE-TOTAL                PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
        05 CF-OCORRENCIAS               OCCURS 010 TIMES.
           10  CFRT-CBCO                PIC S9(003)V     USAGE COMP-3.
           10  CFRT-CAG-BCRIA           PIC S9(005)V     USAGE COMP-3.
           10  CFRT-CCTA-BCRIA-CLI      PIC S9(013)V     USAGE COMP-3.
           10  CFRT-CPRODT              PIC S9(003)V     USAGE COMP-3.
           10  CFRT-CSPROD-DESC-COML    PIC S9(003)V     USAGE COMP-3.
           10  CFRT-DANO-OPER-DESC      PIC S9(004)V     USAGE COMP-3.
           10  CFRT-NSEQ-OPER-DESC      PIC S9(009)V     USAGE COMP-3.
           10  CFRT-NPCELA-DESC-COML    PIC S9(005)V     USAGE COMP-3.
           10  CFRT-CSIT-DESC-COML      PIC S9(003)V     USAGE COMP-3.
           10  CFRT-DVCTO-PCELA-DESC    PIC  X(010).
           10  CFRT-VPRINC-PCELA-DESC   PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-VJURO-PCELA-DESC    PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-VIOF-PCELA-DESC     PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-VSDO-DVDOR-PRINC    PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-VJURO-MORA-GERC     PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-VIOF-PCELA-VENCD    PIC S9(015)V9(2) USAGE COMP-3.
           10  CFRT-QTITLO-CHEQ-PCELA   PIC S9(009)V     USAGE COMP-3.
           10  CFRT-TOPER-DESC-APURA    PIC S9(003)V     USAGE COMP-3.
           10  CFRT-FILLER              PIC  X(100).
