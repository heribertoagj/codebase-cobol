      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0J3 - DB2PRD.TPOSIC_TITLO_ATIVO     *
      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      *****************************************************************
       01  CE-ARGUMENTOS-ENTRADA.

        05 CE-COLUNAS-DA-TABELA.
           10 CE-CBCO                   PIC S9(003)V     USAGE COMP-3.
           10 CE-CAG-BCRIA              PIC S9(005)V     USAGE COMP-3.
           10 CE-CCTA-BCRIA-CLI         PIC S9(013)V     USAGE COMP-3.
           10 CE-CPRODT                 PIC S9(003)V     USAGE COMP-3.
           10 CE-CSPROD-DESC-COML       PIC S9(003)V     USAGE COMP-3.
           10 CE-DANO-OPER-DESC         PIC S9(004)V     USAGE COMP-3.
           10 CE-NSEQ-OPER-DESC         PIC S9(009)V     USAGE COMP-3.
           10 CE-NPCELA-DESC-COML       PIC S9(005)V     USAGE COMP-3.
           10 CE-CSIT-DESC-COML         PIC S9(003)V     USAGE COMP-3.
           10 CE-DVCTO-PCELA-DESC       PIC  X(010).
           10 CE-VPRINC-PCELA-DESC      PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-VJURO-PCELA-DESC       PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-VIOF-PCELA-DESC        PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-VSDO-DVDOR-PCELA       PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-VJURO-MORA-GERC        PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-VIOF-PCELA-VENCD       PIC S9(015)V9(2) USAGE COMP-3.
           10 CE-CCNPJ-CPF              PIC S9(009)V     USAGE COMP-3.
           10 CE-CFLIAL-CNPJ            PIC S9(005)V     USAGE COMP-3.
           10 CE-CCTRL-CNPJ-CPF         PIC S9(002)V     USAGE COMP-3.
           10 CE-IPSSOA-SACDO           PIC  X(060).
           10 CE-CTITLO-COBR-BCO        PIC S9(018)V     USAGE COMP-3.
           10 CE-CSEU-NRO-TITLO         PIC  X(015).
           10 CE-QTITLO-CHEQ-PCELA      PIC S9(009)V     USAGE COMP-3.
           10 CE-FILLER                 PIC  X(100).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
        05 CE-QTDE-A-DESPREZAR          PIC  9(005).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
        05 CE-INSTRUCAO                 PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  CE-RETORNO.
        05 CE-COD-RETORNO               PIC  9(004).
        05 CE-MENSAGEM                  PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
        05 CE-PROXIMO-RESTART           PIC  9(005).
        05 CE-QTDE-RETORNADA            PIC  9(003).
        05 CE-QTDE-TOTAL                PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
        05 CE-OCORRENCIAS               OCCURS 010 TIMES.
           10  CERT-CBCO                PIC S9(003)V     USAGE COMP-3.
           10  CERT-CAG-BCRIA           PIC S9(005)V     USAGE COMP-3.
           10  CERT-CCTA-BCRIA-CLI      PIC S9(013)V     USAGE COMP-3.
           10  CERT-CPRODT              PIC S9(003)V     USAGE COMP-3.
           10  CERT-CSPROD-DESC-COML    PIC S9(003)V     USAGE COMP-3.
           10  CERT-DANO-OPER-DESC      PIC S9(004)V     USAGE COMP-3.
           10  CERT-NSEQ-OPER-DESC      PIC S9(009)V     USAGE COMP-3.
           10  CERT-NPCELA-DESC-COML    PIC S9(005)V     USAGE COMP-3.
           10  CERT-CSIT-DESC-COML      PIC S9(003)V     USAGE COMP-3.
           10  CERT-DVCTO-PCELA-DESC    PIC  X(010).
           10  CERT-VPRINC-PCELA-DESC   PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-VJURO-PCELA-DESC    PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-VIOF-PCELA-DESC     PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-VSDO-DVDOR-PCELA    PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-VJURO-MORA-GERC     PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-VIOF-PCELA-VENCD    PIC S9(015)V9(2) USAGE COMP-3.
           10  CERT-CCNPJ-CPF           PIC S9(009)V     USAGE COMP-3.
           10  CERT-CFLIAL-CNPJ         PIC S9(005)V     USAGE COMP-3.
           10  CERT-CCTRL-CNPJ-CPF      PIC S9(002)V     USAGE COMP-3.
           10  CERT-IPSSOA-SACDO        PIC  X(060).
           10  CERT-CTITLO-COBR-BCO     PIC S9(018)V     USAGE COMP-3.
           10  CERT-CSEU-NRO-TITLO      PIC  X(015).
           10  CERT-QTITLO-CHEQ-PCELA   PIC S9(009)V     USAGE COMP-3.
           10  CERT-FILLER              PIC  X(100).
