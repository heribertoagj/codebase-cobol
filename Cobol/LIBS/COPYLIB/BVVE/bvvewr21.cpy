      *----------------------------------------------------------------*
      *            COMMAREA PARA PROGRAMA BVVEWR21                     *
      *            COMUNICACAO COM A CIP - MENSAGEM RRC0021            *
      *----------------------------------------------------------------*
      *
            05     BVVEWR21-MAPA REDEFINES BVVEWR00-MENSAGEM.
      *            CNPJ ENTIDADE REGISTRADORA
              10   BVVEWR21-CNPJER                          PIC X(014).
      *            IDENTIFICADOR NEGOCIAÁ„O RECEBÌVEL
              10   BVVEWR21-IDENTDNEGCRECBVL                PIC X(040).
      *            IDENTIFICADOR DA OPERAÁ„O
              10   BVVEWR21-IDENTDOP                        PIC X(019).
      *            INDICADOR TIPO NEGOCIAÁ„O
              10   BVVEWR21-INDRTPNEGC                      PIC X(002).
      *            DATA VENCIMENTO OPERAÁ„O
              10   BVVEWR21-DTVENCOP                        PIC X(010).
      *            VALOR TOTAL LIMITE OU SALDO DEVEDOR
              10   BVVEWR21-VLRTOTLIMOUSLDDEVDR-D    PIC 9(009) COMP-4.
              10   BVVEWR21-VLRTOTLIMOUSLDDEVDR-X    PIC X(004).
              10   BVVEWR21-VLRTOTLIMOUSLDDEVDR-9  REDEFINES
                   BVVEWR21-VLRTOTLIMOUSLDDEVDR-X  USAGE COMP-1.
      *            VALOR GARANTIA
              10   BVVEWR21-VLRGAR-D    PIC 9(009) COMP-4.
              10   BVVEWR21-VLRGAR-X    PIC X(004).
              10   BVVEWR21-VLRGAR-9  REDEFINES
                   BVVEWR21-VLRGAR-X  USAGE COMP-1.
      *            INDICADOR GEST„O ENTIDADE REGISTRADORA
              10   BVVEWR21-INDRGESTER                      PIC X(001).
      *            INDICADOR REGRA DIVIS„O
              10   BVVEWR21-INDRREGRDIVS                    PIC X(001).
      *            INDICADOR ALCANCE CONTRATO CREDENCIADORAS/SUBCREDENCI
              10   BVVEWR21-INDCANCONTRCREDDRSUB            PIC X(001).
      *            INDICADOR ACEITE INCONDICIONAL OPERAÁ„O
              10   BVVEWR21-INDRACTEINCONDLOP               PIC X(001).
      *            IDENTIFICADOR CIP DA OPERAÁ„O ORIGINAL RENEGOCIAÁ„O
      *            DIVIDA
              10   BVVEWR21-IDENTDCIPOPORRENEGCDI           PIC X(019).
      *            INDICADOR ACEITE UNIDADE DE RECEBÌVEL RESERVADA
              10   BVVEWR21-INDACTEUNIDRECBVLRESE           PIC X(001).
      *            DATA E HORA INCLUS„O
              10   BVVEWR21-DTHRINCL                        PIC X(024).
      *            INDICADOR DE SITUAÁ„O DA OPERAÁ„O
              10   BVVEWR21-INDRSITOP                       PIC X(001).
      *            INDICADOR AUTORIZAÁ„O CESS„O
              10   BVVEWR21-INDRAUTCCESS                    PIC X(001).
      *            CNPJ OU CPF TITULAR CONTA
              10   BVVEWR21-CNPJOUCPFCESAUTD PIC X(014) OCCURS 10 TIMES.
      *            IDENTIFICADOR OPERAÁ„O ORIGINAL RENEGOCIAÁ„O DIVIDA
              10   BVVEWR21-IDENTDOPORRENEGCDIV      PIC X(019)
                                                     OCCURS 10 TIMES.
      *            UNIDADES RECEBIVEIS - URI
              10   BVVEWR21-UNID-RECEB                      PIC X(256).
      *            UNIDADES RECEBIVEIS A CONSTITUIR - URI
              10   BVVEWR21-UNID-RECEB-A-CONS               PIC X(256).
