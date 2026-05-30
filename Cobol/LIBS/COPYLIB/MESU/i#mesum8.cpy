      *****************************************************
      **                                                 **
      **                 I#MESUM8                        **
      **         INC DO MODULO MESU9018                  **
      **                 LRECL = 463                     **
      **         DATA DE ATUALIZACAO = 10/05/2002-16:07  **
      **                                                 **
      *****************************************************
       01  COMU-DADOS-9018.
           05     COMU-VERSAO            PIC X(006).
           05     COMU-MENSAGEM.
             10   COMU-COD-MENSAGEM      PIC X(003).
             10   COMU-HIFEN             PIC X(001).
             10   COMU-TEXTO-MSG         PIC X(079).
           05     COMU-DADOS-ENTRADA.
             10   COMU-CORR-BANCARIO     PIC X(015).
             10   COMU-COD-EMPR          PIC 9(005).
             10   COMU-COD-DEPDC         PIC 9(005).
             10   COMU-COD-POSTO         PIC 9(005).
           05     COMU-DADOS-SAIDA.
             10   COMU-COD-TIPO-POSTO    PIC 9(002).
             10   COMU-NOME-POSTO        PIC X(020).
             10   COMU-END-LOGDR-POSTO   PIC X(040).
             10   COMU-END-NUM-POSTO     PIC 9(005).
             10   COMU-END-COMPL-POSTO   PIC X(030).
             10   COMU-COD-CEP           PIC 9(005).
             10   COMU-COD-CEP-COMPL     PIC 9(003).
             10   COMU-FUNC-RESP-POSTO   PIC 9(009).
             10   COMU-HOR-INIC-EXPDT    PIC X(008).
             10   COMU-HOR-FINAL-EXPDT   PIC X(008).
             10   COMU-DT-INI-OPER       PIC X(010).
             10   COMU-DT-ONLINE         PIC X(010).
             10   COMU-DT-ENCRR          PIC X(010).
             10   COMU-DT-ULT-INSTA      PIC X(010).
             10   COMU-DTA-POSSE-RESP    PIC X(010).
             10   COMU-DISTR-CORSP       PIC 9(005).
             10   COMU-COD-SETOR-TESOU   PIC X(003).
             10   COMU-COD-TESOU         PIC 9(005).
             10   COMU-COD-EX-LIN-CARRO  PIC 9(001).
             10   COMU-COD-MALOT         PIC X(001).
             10   COMU-TARIF-EMIS-EXTR   PIC 9(001).
             10   COMU-TARIF-CATAO-MAGNT PIC 9(001).
             10   COMU-TARIF-CHEQ-ESPCL  PIC 9(001).
             10   COMU-TARIF-CATAO-CREDT PIC 9(001).
             10   COMU-DT-MUDCA-TPO-PAB  PIC X(010).
             10   COMU-COD-CNPJ-POSTO    PIC 9(009).
             10   COMU-COD-LJ-POSTO      PIC 9(004).
             10   COMU-COD-DIG-CNPJ-POS  PIC 9(002).
             10   COMU-QTD-DIA-VNCLO     PIC 9(002).
             10   COMU-COD-MUN           PIC 9(005).
             10   COMU-COD-COMPE-PAC     PIC 9(003).
             10   COMU-COD-DISTC-AG-BDSC PIC 9(005).
             10   COMU-NOME-CNTAT-POSTO  PIC X(050).
             10   COMU-EMAIL-POSTO       PIC X(040).
             10   COMU-COD-FORN-NUM-POS  PIC X(001).
             10   COMU-COD-RCOLH-NUM-POS PIC X(001).
             10   COMU-COD-DEPDC-NUM-POS PIC 9(005).
             10   COMU-RGIAO-TARIF       PIC 9(005).
             10   COMU-QTD-DIA-ENVIO-LOG PIC 9(003).
