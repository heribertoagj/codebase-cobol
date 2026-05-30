      *****************************************************************
      * NOME DA INC - GFCTWAG2                                        *
      * DESCRICAO   - CANCELAMENTO DE EMISSAO DE EXTRATO ANUAL        *
      *               DE TARIFAS VIA CORREIO - POR AGRUPAMENTO   INCL.*
      *                                                               *
      * WAG2-LL     - 630                                             *
      * DATA        - MAIO / 2009                                     *
      * RESPONSAVEL - HELIO SANTONI - PROCWORK                        *
      *****************************************************************
      *
       01  WAG2-ENTRADA.
           03 WAG2-LL                            PIC S9(004) COMP.
           03 WAG2-ZZ                            PIC S9(004) COMP.
           03 WAG2-TRANSACAO                     PIC  X(009).
           03 WAG2-FUNCAO                        PIC  X(008).
           03 WAG2-DADOS.
              05 WAG2-FUNC-BDSCO                 PIC  X(007).
              05 WAG2-FIM                        PIC  X(001).
           03 WAG2-CAMPOS.
              05 WAG2-CAGPTO                     PIC  9(003).
              05 WAG2-CSEGMTO                    PIC  9(003).
              05 WAG2-CAGEN                      PIC  9(005).
              05 WAG2-CPAB                       PIC  9(005).
              05 WAG2-CCNPJ-CPF.
                 10 WAG2-NUMERO                  PIC  9(009).
                 10 WAG2-FILIAL                  PIC  9(005).
                 10 WAG2-CONTRL                  PIC  9(002).
              05 WAG2-JUSTIF                     PIC  X(254).
           03 FILLER                             PIC  X(315).
