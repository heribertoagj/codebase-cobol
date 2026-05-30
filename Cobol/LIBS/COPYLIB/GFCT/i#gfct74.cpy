      *****************************************************************
      * NOME DA INC - I#GFCT74                                        *
      * DESCRICAO   - GESTOR LOGADO                                   *
      * TAMANHO     - 131                                             *
      * GFCT74-LL   - 131                                             *
      * DATA        - NOVEMBRO/2007                                   *
      * RESPONSAVEL - VALERIA TORQUATO - PROCWORK - CLONE DA INC F9   *
      *****************************************************************
      *
       01  GFCT74-GFCT5578.
           03 GFCT74-AREA-ENTRADA.
              05 GFCT74-AGENCIA                    PIC  9(005).
              05 GFCT74-CONTA                      PIC  9(007).
              05 GFCT74-COD-FUNC                   PIC  X(007).
           03 GFCT74-AREA-SAIDA.
              05 GFCT74-COD-RETORNO                PIC  9(002).
      *           00 - TIPO DE DEPENDENCIA OK
      *           02 - ERRO NO RETORNO DA JUNÇ O DO SOLICITANTE - GFCT55
      *           03 - SOMENTE A PROPRIA AGENCIA PODE SER SOLICITANTE
      *           04 - ERRO NO RETORNO DA IDENTIFICAÇ O DE MASTER -GFCT5
      *           06 - ERRO NO ACESSO AO MODULO MESU9023 - DIRETORIA REG
      *           07 - ERRO NO ACESSO AO MODULO MESU9023 - GERENCIA REGI
      *           08 - ERRO NO ACESSO AO MODULO CLIE9018 - TIPO SEGMENTO
      *           09 - ERRO NO ACESSO AO MODULO CLIE9018 - DEPTO SEGMENT
      *           10 - USUARIO SEM PERFIL CADASTRADO
      *           11 - DADOS INCONSISTENTES
              05 GFCT74-COD-SQL-ERRO               PIC  9(003).
              05 GFCT74-DESC-MSG                   PIC  X(079).
              05 GFCT74-DADOS-RETORNO.
                 07 GFCT74-TIPO-DEPENDENCIA        PIC  X(002).
                 07 GFCT74-DPTO                    PIC  9(005).
