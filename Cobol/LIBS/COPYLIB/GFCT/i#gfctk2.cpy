      *****************************************************************
      * NOME DA INC - I#GFCTK2                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - LUPA - GFCT5116  *
      *               COMUNICACAO WEB X IMS        - SAIDA            *
      * TAMANHO     - 300                                             *
      * GFCTK2-LL   - 300                                             *
      * DATA        - 02.02.2006                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *****************************************************************
      *
       01  GFCTK2-SAIDA.
           03  GFCTK2-LL                            PIC S9(004) COMP.
           03  GFCTK2-ZZ                            PIC S9(004) COMP.
           03  GFCTK2-TRANSACAO                     PIC  X(009).
           03  GFCTK2-FUNCAO                        PIC  X(008).
           03  GFCTK2-DADOS-RST.
               05  GFCTK2-QTDE-OCOR                 PIC  9(003).
               05  GFCTK2-QTDE-TOT-REG              PIC  9(006).
               05  GFCTK2-FUNC-BDSCO                PIC  X(007).
               05  GFCTK2-TERMINAL                  PIC  X(008).
               05  GFCTK2-DADOS1.
                   07  GFCTK2-AGENCIA               PIC  9(005).
                   07  GFCTK2-CONTA                 PIC  9(007).
           03  GFCTK2-AREA-SAIDA.
               05  GFCTK2-COD-RETORNO               PIC  9(002).
      *
      * 00 - TIPO DE DEPENDENCIA OK
      * 02 - ERRO NO RETORNO DA JUNCAO DO SOLICITANTE - GFCT5505
      * 03 - SOMENTE A PROPRIA AGENCIA PODE SER SOLICITANTE
      * 04 - ERRO NO RETORNO DA IDENTIFICACAO DE MASTER -GFCT5507
      * 06 - ERRO NO ACESSO AO MODULO MESU9023 - DIRETORIA REGIONAL
      * 07 - ERRO NO ACESSO AO MODULO MESU9023 - GERENCIA REGIONAL
      * 08 - ERRO NO ACESSO AO MODULO CLIE9018 - TIPO SEGMENTO
      * 09 - ERRO NO ACESSO AO MODULO CLIE9018 - DEPTO SEGMENTO
      * 10 - USUARIO SEM PERFIL CADASTRADO
      * 11 - DADOS INCONSISTENTES
      *
               05  GFCTK2-ERRO                      PIC  9(001).
               05  GFCTK2-COD-MSG                   PIC  9(007).
               05  GFCTK2-DESC-MSG                  PIC  X(079).
               05  GFCTK2-COD-SQL-ERRO              PIC  9(003).
               05  GFCTK2-DADOS-RETORNO.
                  07  GFCTK2-TIPO-DEPENDENCIA       PIC  X(002).
                  07  GFCTK2-DPTO                   PIC  9(005).
               05  GFCTK2-FIM                       PIC  X(001).
               05  GFCTK2-NOME-CLIENTE              PIC  X(040).
               05  FILLER                           PIC  X(103).
