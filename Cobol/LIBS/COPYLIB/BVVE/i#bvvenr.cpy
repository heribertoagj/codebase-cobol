      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEA800                     *
      *            ENVIA SOLICITACAO DE INCLUSAO DE OPTIN              *
      *----------------------------------------------------------------*
      *
       01 BVVENR-COMMAREA.
          03  BVVENR-NCADTO-AUTRZ-AGNDA    PIC 9(013).
          03  BVVENR-CNPJ-CPF-USU-FIN.
              05 BVVENR-CNPJ-USUAR         PIC 9(009).
              05 BVVENR-FILIAL-USUAR       PIC 9(004).
              05 BVVENR-CTRL-CNPJ          PIC 9(002).
          03  BVVENR-CAG-BCRIA             PIC 9(005).
          03  BVVENR-CCTA-BCRIA            PIC 9(013).
          03  BVVENR-CCTA-BCRIA-R          REDEFINES
              BVVENR-CCTA-BCRIA.
              05 BVVENR-CCTA-BCRIA-N       PIC 9(012).
              05 BVVENR-DIG-CCTA-BCRIA-N   PIC 9(001).
          03  BVVENR-COD-CREDENC           PIC 9(004).
          03  BVVENR-CNPJ-FINCDR           PIC 9(014).
          03  BVVENR-COD-BAND-CORP         PIC 9(003).
          03  BVVENR-DT-ASSINATURA         PIC X(010).
          03  BVVENR-DT-INI-OPTIN          PIC X(010).
          03  BVVENR-DT-FIM-OPTIN          PIC X(010).
          03  BVVENR-INDR-DOMCL            PIC X(001).
          03  BVVENR-USUARIO               PIC X(008).
          03  BVVENR-CTPO-ACAO-AUTRZ       PIC X(001).
          03  BVVENR-CCNPJ-CRDRA-ESTBL     PIC 9(009).
          03  BVVENR-CFLIAL-CRDRA-ESTBL    PIC 9(004).
          03  BVVENR-CCTRL-CRDRA-ESTBL     PIC 9(002).
          03  BVVENR-CBANDE-VSAO-EXTER     PIC X(005).
          03  BVVENR-SAIDA.
              05 BVVENR-COD-RETORNO        PIC  9(005).
              05 BVVENR-MENSAGEM           PIC  X(079).

      *   QUANDO PF, ENVIAR O BVVENR-FILIAL-USUAR COM ZEROS
      *   BVVENR-NCADTO-AUTRZ-AGNDA DA TABELA BVVEB049
      *   BVVENR-CTPO-ACAO-AUTRZ "I" OPTIN OU "E" OPTOUT

