      *****************************************************************
      * NOME DA INC - I#GFCTIQ                                        *
      * DESCRICAO   - UNIFICACAO DE DEBITOS - CNPJ/CPF                *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 300                                             *
      * DATA        - DEZ.2005                                        *
      * RESPONSAVEL - RENATO   - PROCWORK           4152-4352         *
      *****************************************************************
      *
       01  GFCTIQ-ENTRADA.
           03  GFCTIQ-LL                            PIC S9(004) COMP.
           03  GFCTIQ-ZZ                            PIC S9(004) COMP.
           03  GFCTIQ-TRANSACAO                     PIC  X(009).
           03  GFCTIQ-FUNCAO                        PIC  X(008).
           03  GFCTIQ-DADOS-RST.
               05 GFCTIQ-FILTRO.
                   07 GFCTIQ-CPF-CNPJ               PIC  9(009).
                   07 GFCTIQ-FILIAL                 PIC  9(005).
                   07 GFCTIQ-CTRL                   PIC  9(002).
               05  GFCTIQ-QTDE-OCOR                 PIC  9(003).
               05  GFCTIQ-QTDE-TOT-REG              PIC  9(006).
               05  GFCTIQ-FUNC-BDSCO                PIC  9(007).
               05  GFCTIQ-DPTO                      PIC  9(005).
               05  GFCTIQ-MASTER                    PIC  X(001).
               05  GFCTIQ-TERMINAL                  PIC  X(008).
               05  GFCTIQ-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(057).
           03  FILLER OCCURS 5 TIMES.
               05 GFCTIQ-TARIFA                     PIC  9(005).
               05 GFCTIQ-DESC-TARIFA                PIC  X(030).
