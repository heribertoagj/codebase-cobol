      *****************************************************************
      * NOME DA INC - I#GFCTIS                                        *
      * DESCRICAO   - UNIFICACAO DE DEBITO                            *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 812                                             *
      * DATA        - DEZ.2005                                        *
      * RESPONSAVEL - RENATO   - PROCWORK           4152-4352         *
      *****************************************************************
      *
       01  GFCTIS-ENTRADA.
           03  GFCTIS-LL                            PIC S9(004) COMP.
           03  GFCTIS-ZZ                            PIC S9(004) COMP.
           03  GFCTIS-TRANSACAO                     PIC  X(009).
           03  GFCTIS-FUNCAO                        PIC  X(008).
           03  GFCTIS-DADOS-RST.
             05  GFCTIS-FILTRO.
                   07 GFCTIS-CPF-CNPJ               PIC  9(009).
                   07 GFCTIS-FILIAL                 PIC  9(005).
                   07 GFCTIS-CTRL                   PIC  9(002).
             05  GFCTIS-NOME                        PIC  X(030).
             05  GFCTIS-QTDE-OCOR                   PIC  9(003).
             05  GFCTIS-QTDE-TOT-REG                PIC  9(006).
             05  GFCTIS-FUNC-BDSCO                  PIC  9(007).
             05  GFCTIS-PONTEIRO.
                   07 GFCTIS-CPF-CNPJ-P             PIC  9(009).
                   07 GFCTIS-FILIAL-P               PIC  9(005).
                   07 GFCTIS-TARIFA-P               PIC  9(005).
                   07 GFCTIS-TIMESTAMP              PIC  X(026).
             05  GFCTIS-DPTO                        PIC  9(005).
             05  GFCTIS-MASTER                      PIC  X(001).
             05  GFCTIS-TERMINAL                    PIC  X(008).
             05  GFCTIS-ACAO                        PIC  X(001).
      *          A-ATIVOS   C-CONSULTA GERAL
             05  GFCTIS-FIM                         PIC  X(001).
             05  FILLER                             PIC  X(020).
           03  FILLER OCCURS 6 TIMES.
             05  GFCTIS-TARIFA                      PIC  9(005).
             05  GFCTIS-DESC-TARIFA                 PIC  X(030).
             05  GFCTIS-SIT                         PIC  9(001).
      *          1-ATIVO   2-CANCELADO
             05  GFCTIS-DT-INI                      PIC  X(010).
      *          DATA FORMATO DD.MM.AAAA
             05  GFCTIS-DT-FIM                      PIC  X(010).
      *          DATA FORMATO DD.MM.AAAA
             05  GFCTIS-DT-INI-TIMESTAMP            PIC  X(026).
             05  GFCTIS-DT-FIM-TIMESTAMP            PIC  X(026).
