      ******************************************************************
      * NOME DA INC - I#GFCTJL                                         *
      * DESCRICAO   - MODULO PARA RECUPERAR O CODIGO DE DEPENDENCIA    *
      * TAMANHO     - 050                                              *
      * DATA        - 17/01/2006                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTJL-AREA.
           03  GFCTJL-AREA-ENVIO.
               05  GFCTJL-TPO-AMBIENTE           PIC  X(001).
               05  GFCTJL-CIDTFD-DEPDC           PIC  X(002).
               05  GFCTJL-CJUNC-DEPDC            PIC  9(005).
               05  GFCTJL-CCTA-CLI               PIC  9(007).
               05  GFCTJL-CSERVC-TARIF           PIC  9(005).
           03  GFCTJL-AREA-RETORNO.
               05  GFCTJL-COD-RETORNO            PIC  9(002).
      *            00 - TERMINO COM SUCESSO
      *            01 - TIPO DE AMBIENTE DE PROCESSAMENTO INVALIDO
      *            02 - ARGUMENTO(S) DE PESQUISA INVALIDO(S)
      *            03 - JUNCAO NAO ENCONTRADA P/ GESTOR MASTER
      *            04 - JUNCAO NAO ENCONTRADA P/ GERENCIA REGIONAL
      *            05 - JUNCAO NAO ENCONTRADA P/ DIRETORIA REGIONAL
      *            06 - ARGUMENTO(S) DE PESQUISA INVALIDO(S) P/ MESU9023
      *            07 - ERRO APL NO ACESSO AO MODULO MESU9023
      *            08 - ARGUMENTO(S) DE PESQUISA INVALIDO(S) P/ GFCT5534
      *            09 - JUNCAO NAO ENCONTRADA P/ DEPARTAMENTO SEGMENTO
      *            10 - ERRO APL NO ACESSO AO MODULO GFCT5534
      *            99 - TERMINO COM ERRO DE DB2
               05  GFCTJL-COD-SQL-ERRO           PIC  9(003).
               05  GFCTJL-CDEPDC                 PIC  9(005).
           03  FILLER                            PIC  X(020).
