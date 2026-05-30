      *****************************************************************
      * NOME DA INC - GFCTWAGQ                                        *
      * DESCRICAO   - AUTORIZACAO DE EMISSAO DE EXTRATO ANUAL         *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   EXCL.*
      *                                                               *
      * WAGQ-LL     - 650                                             *
      * DATA        - MAIO / 2009                                     *
      * RESPONSAVEL - HELIO SANTONI - PROCWORK                        *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  WAGQ-ENTRADA.
           03 WAGQ-LL                            PIC S9(004) COMP.
           03 WAGQ-ZZ                            PIC S9(004) COMP.
           03 WAGQ-TRANSACAO                     PIC  X(009).
           03 WAGQ-FUNCAO                        PIC  X(008).
           03 WAGQ-DADOS.
              05 WAGQ-FUNC-BDSCO                 PIC  X(007).
              05 WAGQ-FIM                        PIC  X(001).
           03 WAGQ-CAMPOS.
              05 WAGQ-HINCL                      PIC  X(026).
              05 WAGQ-CAGPTO                     PIC  9(003).
              05 WAGQ-CSEGMTO                    PIC  9(003).
              05 WAGQ-CAGEN                      PIC  9(005).
              05 WAGQ-CPAB                       PIC  9(005).
              05 WAGQ-CCNPJ-CPF.
ST2507*          10 WAGQ-NUMERO                  PIC  9(009).
ST2507*          10 WAGQ-FILIAL                  PIC  9(005).
ST2507           10 WAGQ-NUMERO                  PIC  X(009).
ST2507           10 WAGQ-FILIAL                  PIC  X(004).
                 10 WAGQ-CONTRL                  PIC  9(002).
              05 WAGQ-JUSTIF                     PIC  X(254).
           03 FILLER                             PIC  X(309).
