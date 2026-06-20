      *****************************************************************
      * NOME DA INC - I#GFCTOI                                        *
      * DESCRICAO   - EXCECAO ADESAO COMPULSORIA - COMBO ARGUMENTOS   *
      *               COMUNICACAO IMS X WEB - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * GFCTOI-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      *****************************************************************
       01  GFCTOI-SAIDA.
           03 GFCTOI-LL                            PIC S9(004) COMP.
           03 GFCTOI-ZZ                            PIC S9(004) COMP.
           03 GFCTOI-TRANSACAO                     PIC  X(009).
           03 GFCTOI-FUNCAO                        PIC  X(008).
           03 GFCTOI-DADOS-RST.
              05 GFCTOI-QTDE-OCOR                  PIC  9(003).
              05 GFCTOI-QTDE-TOT-REG               PIC  9(006).
              05 GFCTOI-FUNC-BDSCO                 PIC  X(007).
              05 GFCTOI-FILTRO.
                 07 GFCTOI-FILTRO-PACOTE           PIC  9(005).
                 07 GFCTOI-FILTRO-AGPTO            PIC  9(003).
              05 GFCTOI-PONTEIRO.
                 07 GFCTOI-PONT-PACOTE             PIC  9(005).
                 07 GFCTOI-PONT-AGPTO              PIC  9(003).
                 07 GFCTOI-PONT-SEQ                PIC  9(009).
              05 GFCTOI-ERRO                       PIC  9(001).
              05 GFCTOI-COD-MSG-ERRO               PIC  9(007).
              05 GFCTOI-COD-SQL-ERRO               PIC  9(003).
              05 GFCTOI-DESC-MSG-ERRO              PIC  X(079).
              05 GFCTOI-FIM                        PIC  X(001).
              05 FILLER                            PIC  X(161).
           03 GFCTOI-OCORR                         OCCURS 8 TIMES.
              05 GFCTOI-SEQUENCIA                  PIC  9(009).
ST2506*       05 GFCTOI-CPF-CNPJ                   PIC  9(009).
ST2506*       05 GFCTOI-FILIAL                     PIC  9(005).
ST2506        05 GFCTOI-CPF-CNPJ                   PIC  X(009).
ST2506        05 GFCTOI-FILIAL                     PIC  X(004).
              05 GFCTOI-CONTROLE                   PIC  9(002).
              05 GFCTOI-AGENCIA                    PIC  9(005).
              05 GFCTOI-POSTO                      PIC  9(005).
              05 GFCTOI-SEGMENTO                   PIC  9(003).
              05 GFCTOI-MUNICIPIO                  PIC  9(007).
              05 GFCTOI-UF                         PIC  X(002).
              05 GFCTOI-DESC-NOME                  PIC  X(040).
