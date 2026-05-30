      ******************************************************************
      * NOME DA INC - I#GFCTIU                                         *
      * DESCRICAO   - TEXT DE NOME DO TITULAR CPF/CNPJ                 *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 100                                              *
      * DATA        - 23/12/2005                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTIU-ENTRADA.
           03  GFCTIU-LL                         PIC S9(004) COMP.
           03  GFCTIU-ZZ                         PIC S9(004) COMP.
           03  GFCTIU-TRANSACAO                  PIC  X(009).
           03  GFCTIU-FUNCAO                     PIC  X(008).
           03  GFCTIU-DADOS-RST.
               05  GFCTIU-QTDE-OCOR              PIC  9(003).
               05  GFCTIU-QTDE-TOT-REG           PIC  9(006).
               05  GFCTIU-FUNC-BDSCO             PIC  X(007).
               05  GFCTIU-FIM                    PIC  X(001).
ST2506*        05  GFCTIU-NRO-CPF-CNPJ           PIC  9(009).
ST2506*        05  GFCTIU-NRO-FILIAL             PIC  9(005).
ST2506         05  GFCTIU-NRO-CPF-CNPJ           PIC  X(009).
ST2506         05  GFCTIU-NRO-FILIAL             PIC  X(004).
               05  GFCTIU-CTRL-CPF-CNPJ          PIC  9(002).
           03  FILLER                            PIC  X(046).
