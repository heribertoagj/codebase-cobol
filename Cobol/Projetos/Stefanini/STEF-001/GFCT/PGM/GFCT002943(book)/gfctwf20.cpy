      *****************************************************************
      * NOME DA INC - GFCTWF20                                        *
      * DESCRICAO   - AREA DE SAIDA CONSULTA                          *
      *               INVESTIMENTOS ULTIMOS 03 MESES                  *
      *                                                               *
      * WF20-LL     - 1010                                            *
      * DATA        - OUT/2015                                        *
      * WF20-FILIAL-CGCCPF = ZEROS ENTAO CPF SE NAO CNPJ              *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WF20-SAIDA.
           03 WF20-LL                            PIC S9(004) COMP.
           03 WF20-ZZ                            PIC S9(004) COMP.
           03 WF20-TRANSACAO                     PIC  X(009).
           03 WF20-FUNCAO                        PIC  X(008).
           03 WF20-FUNC-BDSCO                    PIC  X(007).
           03 WF20-FIM                           PIC  X(001).
           03 WF20-ERRO                          PIC  9(001).
           03 WF20-COD-MSG-ERRO                  PIC  9(007).
           03 WF20-COD-SQL-ERRO                  PIC  9(003).
           03 WF20-DESC-MSG-ERRO                 PIC  X(079).
           03 WF20-CAMPOS.
              05 WF20-CAG-BCRIA                  PIC  9(005).
              05 WF20-CCTA-CLI                   PIC  9(007).
              05 WF20-DANO-MES-SDO               PIC  9(006).
ST2507*       05 WF20-NUM-CGCCPF                 PIC  9(009).
ST2507*       05 WF20-FILIAL-CGCCPF              PIC  9(004).
ST2507        05 WF20-NUM-CGCCPF                 PIC  X(009).
ST2507        05 WF20-FILIAL-CGCCPF              PIC  X(004).
              05 WF20-CONTR-CGCCPF               PIC  9(002).
              05 WF20-NOME                       PIC  X(040).
              05 WF20-CSERVC-TARIF               PIC  9(006).
              05 WF20-VSDO-MED-FUNDO             PIC  9(015)V99.
              05 WF20-VSDO-MED-POUP              PIC  9(015)V99.
              05 WF20-VSDO-MED-CERTF             PIC  9(015)V99.
              05 WF20-VSDO-MED-PREVD             PIC  9(015)V99.
              05 WF20-VSDO-APLIC-ACOES           PIC  9(015)V99.
              05 WF20-VSDO-APLIC-TSORO           PIC  9(015)V99.
              05 WF20-VSDO-APLIC-DBTRS           PIC  9(015)V99.
              05 WF20-VSDO-APLIC-DERIV           PIC  9(015)V99.
           03 WF20-QTDE-OCCUR                    PIC  9(001).
           03 WF20-COMBO.
              05 FILLER                          OCCURS 3.
                 07 WF20-DANO-MES                PIC  9(006).
           03 WF20-FILLER                        PIC  X(657).
