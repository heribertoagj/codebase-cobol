      *----------------------------------------------------------------*
      *            HEADER DA COMMAREA PARA COMUNICACAO COM A CIP       *
      *            COMUNICACAO COM A CIP - HEADER E PARAMETROS         *
      *            TAMANHO = 0300                                      *
      *----------------------------------------------------------------*
      *
       02          BVVEWR00-COMMAREA.
         03        BVVEWR00-COMMAREA-TESTE.
      *            NOME DA BOOK UTILIZADA  (EX: BVVEWE10)
            05     BVVEWR00-LEIAUTE           PIC X(008).
      *            IDENTIFICADOR DE MENSAGEM PARA O NEGOCIO
      *            --- MSGS SINCRONAS IRAO ENVIAR ESSE ID NO RETORNO
            05     BVVEWR00-ID                PIC X(020).
      *            CODIGO DO HTTP DE RETORNO
            05     BVVEWR00-CODHTTP           PIC 9(003).
      *            NUMERO DA PAGINA DE RETORNO
            05     BVVEWR00-PAGINA            PIC 9(005).
      *            TOTAL DE PAGINAS RETORNADAS
            05     BVVEWR00-TOT-PAG           PIC 9(005).
            05     FILLER OCCURS 05 TIMES.
      *            CÛDIGO DA MENSAGEM.
              10   BVVEWR00-CODIGO            PIC X(010).
      *            CONTEUDO DO CAMPO QUE ESTA COM ERRO.
              10   BVVEWR00-CONTEUDO          PIC X(100).
      *            CAMPO QUE ESTA COM ERRO.
              10   BVVEWR00-CAMPO             PIC X(050).
      *            DESCRIÁ„O DA MENSAGEM
              10   BVVEWR00-MENSAGEM-RET      PIC X(200).
      *            AREA DA MENSAGEM A SER ENVIADA
            05     BVVEWR00-MENSAGEM          PIC X(28000).
