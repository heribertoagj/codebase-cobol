      *****************************************************************
      * NOME DA INC - I#GFCTF9                                        *
      * DESCRICAO   - GESTOR LOGADO                                   *
      * TAMANHO     - 131                                             *
      * GFCTF9-LL   - 131                                             *
      * DATA        - AGOSTO/2005                                     *
      * RESPONSAVEL - RENATO TAMANAHA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTF9-GFCT5515.
           03 GFCTF9-AREA-ENTRADA.
              05 GFCTF9-AGENCIA                    PIC  9(005).
              05 GFCTF9-CONTA                      PIC  9(007).
              05 GFCTF9-COD-FUNC                   PIC  X(007).
           03 GFCTF9-AREA-SAIDA.
              05 GFCTF9-COD-RETORNO                PIC  9(002).
      *           00 - TIPO DE DEPENDENCIA OK
      *           02 - ERRO NO RETORNO DA JUNÇ O DO SOLICITANTE - GFCT55
      *           03 - SOMENTE A PROPRIA AGENCIA PODE SER SOLICITANTE
      *           04 - ERRO NO RETORNO DA IDENTIFICAÇ O DE MASTER -GFCT5
      *           06 - ERRO NO ACESSO AO MODULO MESU9023 - DIRETORIA REG
      *           07 - ERRO NO ACESSO AO MODULO MESU9023 - GERENCIA REGI
      *           08 - ERRO NO ACESSO AO MODULO CLIE9018 - TIPO SEGMENTO
      *           09 - ERRO NO ACESSO AO MODULO CLIE9018 - DEPTO SEGMENT
      *           10 - USU RIO SEM PERFIL CADASTRADO
      *           11 - DADOS INCONSISTENTES
              05 GFCTF9-COD-SQL-ERRO               PIC  9(003).
              05 GFCTF9-DESC-MSG                   PIC  X(079).
              05 GFCTF9-DADOS-RETORNO.
                 07 GFCTF9-TIPO-DEPENDENCIA        PIC  X(002).
                 07 GFCTF9-DPTO                    PIC  9(005).
