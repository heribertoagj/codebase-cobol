      *****************************************************************
      * NOME DA INC - I#GFCTIT                                        *
      * DESCRICAO   - UNIFICACAO DE DEBITO                            *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - DEZ.2005                                        *
      * RESPONSAVEL - RENATO   - PROCWORK           4152-4352         *
      *****************************************************************
      *
       01  GFCTIT-SAIDA.
           03  GFCTIT-LL                            PIC S9(004) COMP.
           03  GFCTIT-ZZ                            PIC S9(004) COMP.
           03  GFCTIT-TRANSACAO                     PIC  X(009).
           03  GFCTIT-FUNCAO                        PIC  X(008).
           03  GFCTIT-FILTRO.
               05 GFCTIT-CPF-CNPJ                   PIC  9(009).
               05 GFCTIT-FILIAL                     PIC  9(005).
               05 GFCTIT-CTRL                       PIC  9(002).
           03  GFCTIT-NOME                          PIC  X(030).
           03  GFCTIT-QTDE-OCOR                     PIC  9(003).
           03  GFCTIT-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTIT-FUNC-BDSCO                    PIC  9(007).
           03  GFCTIT-PONTEIRO.
               05 GFCTIT-CPF-CNPJ-P                 PIC  9(009).
               05 GFCTIT-FILIAL-P                   PIC  9(005).
               05 GFCTIT-TARIFA-P                   PIC  9(005).
               05 GFCTIT-TIMESTAMP                  PIC  X(026).
           03  GFCTIT-DPTO                          PIC  9(005).
           03  GFCTIT-MASTER                        PIC  X(001).
           03  GFCTIT-TERMINAL                      PIC  X(008).
           03  GFCTIT-ACAO                          PIC  X(001).
      *          A-ATIVOS   C-CONSULTA GERAL
           03  GFCTIT-ERRO                          PIC  9(001).
           03  GFCTIT-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTIT-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTIT-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTIT-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(128).
           03  GFCTIT-DADOS.
               05  FILLER OCCURS 6 TIMES.
                   07  GFCTIT-TARIFA                PIC  9(005).
                   07  GFCTIT-DESC-TARIFA           PIC  X(030).
                   07  GFCTIT-SIT                   PIC  9(001).
      *            1-ATIVO   2-CANCELADO
                   07  GFCTIT-DT-INI                PIC  X(010).
      *            DATA FORMATO DD.MM.AAAA
                   07  GFCTIT-DT-FIM                PIC  X(010).
      *            DATA FORMATO DD.MM.AAAA
                   07  GFCTIT-DT-INI-TIMESTAMP      PIC  X(026).
                   07  GFCTIT-DT-FIM-TIMESTAMP      PIC  X(026).
