      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEA802                     *
      *            ENVIA SOLICITACAO DE ENVIO DE OPTOUT                *
      *----------------------------------------------------------------*
      *
       01 BVVENS-COMMAREA.
          03  BVVENS-NCADTO-AUTRZ-AGNDA    PIC 9(013).
          03  BVVENS-CNPJ-CPF-USU-FIN.
              05 BVVENS-CNPJ-USUAR         PIC 9(009).
              05 BVVENS-FILIAL-USUAR       PIC 9(004).
              05 BVVENS-CTRL-CNPJ          PIC 9(002).
          03  BVVENS-CAG-BCRIA             PIC 9(005).
          03  BVVENS-CCTA-BCRIA            PIC 9(013).
          03  BVVENS-CCTA-BCRIA-R          REDEFINES
              BVVENS-CCTA-BCRIA.
              05 BVVENS-CCTA-BCRIA-N       PIC 9(012).
              05 BVVENS-DIG-CCTA-BCRIA-N   PIC 9(001).
          03  BVVENS-COD-CREDENC           PIC 9(004).
          03  BVVENS-CNPJ-FINCDR           PIC 9(014).
          03  BVVENS-COD-BAND-CORP         PIC 9(003).
          03  BVVENS-USUARIO               PIC X(008).
          03  BVVENS-CTPO-ACAO-AUTRZ       PIC X(001).
          03  BVVENS-SAIDA.
              05 BVVENS-COD-RETORNO        PIC  9(005).
              05 BVVENS-MENSAGEM           PIC  X(079).

      *   QUANDO PF, ENVIAR O BVVENS-FILIAL-USUAR COM ZEROS
      *   BVVENS-CTPO-ACAO-AUTRZ "I" OPTIN OU "E" OPTOUT
      *   BVVENR-NCADTO-AUTRZ-AGNDA DA TABELA BVVEB049

