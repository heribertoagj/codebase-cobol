***************************** Top of Data ******************************
      ******************************************************************
      * NOME DA INC - I#GFCTRK                                         *
      * DESCRICAO   - MODULO PARA RECUPERAR O CODIGO DO MUNICIPIO, E   *
      *               A SIGLA DA UF DA AGENCIA                         *
      * TAMANHO     - 050                                              *
      * DATA        - 02/05/2006                                       *
      * RESPONSAVEL - FRANCISCO LOURENCO - PROCWORK - TEL 4152-4352    *
      ******************************************************************
      *
       01  GFCTRK-AREA.
           03  GFCTRK-AREA-ENVIO.
               05  GFCTRK-TPO-AMBIENTE           PIC  X(001).
               05  GFCTRK-CJUNC-DEPDC            PIC  9(005).
           03  GFCTRK-AREA-RETORNO.
               05  GFCTRK-COD-RETORNO            PIC  9(002).
      *            00 - TERMINO COM SUCESSO
      *            01 - TIPO DE AMBIENTE DE PROCESSAMENTO INVALIDO
      *            02 - ARGUMENTO DE PESQUISA INVALIDO
      *            03 - CODIGO DO MUNICIPIO/SIGLA DA UF DA AGENCIA
      *                 NAO ENCONTRADOS
      *            99 - TERMINO COM ERRO DE DB2
               05  GFCTRK-COD-SQL-ERRO           PIC  9(003).
               05  GFCTRK-CMUN-RURAL             PIC  9(009).
               05  GFCTRK-CSGL-UF                PIC  X(002).
           03  FILLER                            PIC  X(028).
