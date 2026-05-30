      ******************************************************************
      * NOME DA INC - I#GFCTJM                                         *
      * DESCRICAO   - MODULO PARA RECUPERAR O CODIGO DE DEPENDENCIA    *
      *               DE GESTAO DO SEGMENTO DA TARIFA                  *
      * TAMANHO     - 050                                              *
      * DATA        - 17/01/2006                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTJM-AREA.
           03  GFCTJM-AREA-ENVIO.
               05  GFCTJM-TPO-AMBIENTE           PIC  X(001).
               05  GFCTJM-CJUNC-DEPDC            PIC  9(005).
               05  GFCTJM-CCTA-CLI               PIC  9(007).
           03  GFCTJM-AREA-RETORNO.
               05  GFCTJM-COD-RETORNO            PIC  9(002).
      *            00 - TERMINO COM SUCESSO
      *            01 - TIPO DE AMBIENTE DE PROCESSAMENTO INVALIDO
      *            02 - ARGUMENTO(S) DE PESQUISA INVALIDO(S)
      *            03 - ASSOCIACAO DO SEGMENTO DO CLIENTE NAO ENCONTRADO
      *            05 - ARGUMENTO(S) DE PESQUISA INVALIDO(S) P/ GFCT5532
      *            06 - SEGMENTO DO CLIENTE NAO ENCONTRADO
      *            07 - ERRO APL NO ACESSO AO MODULO GFCT5532
      *            99 - TERMINO COM ERRO DE DB2
               05  GFCTJM-COD-SQL-ERRO           PIC  9(003).
               05  GFCTJM-CDEPDC                 PIC  9(005).
               05  GFCTJM-CSGMTO-GSTAO-TARIF     PIC  9(003).
               05  GFCTJM-CTPO-PSSOA             PIC  X(001).
           03  FILLER                            PIC  X(023).
