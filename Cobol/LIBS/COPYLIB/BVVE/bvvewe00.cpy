      *----------------------------------------------------------------*
      *            HEADER DA COMMAREA PARA COMUNICACAO COM A CIP       *
      *            COMUNICACAO COM A CIP - HEADER E PARAMETROS         *
      *            TAMANHO = 0300                                      *
      *----------------------------------------------------------------*
      *
       02          BVVEWE00-COMMAREA.
            05 BVVEWE00-HEADER.
              10 BVVEWE00-COD-LAYOUT     PIC X(08)   VALUE 'BVVEWE00'.
              10 BVVEWE00-TAM-LAYOUT     PIC 9(05)   VALUE 22000.
      *            NOME DA BOOK UTILIZADA  (EX: BVVEWE10)
            05     BVVEWE00-LEIAUTE           PIC X(008).
      *            IDENTIFICADOR DE MENSAGEM PARA O NEGOCIO
      *            --- MSGS SINCRONAS IRAO ENVIAR ESSE ID NO RETORNO
            05     BVVEWE00-ID                PIC X(020).
      *            ID DO TERMINAL - OBTIDO ATRAVES DO COMANDO:
      *            EXEC CICS ASSIGN NETNAME
            05     BVVEWE00-TERMID            PIC X(008).
      *            CODIGO DE RETORNO
      *            08 = ERRO INTERNO
      *            10 = ERRO DE COMUNICAÁ„O
      *            12 = ERRO DA CIP (VERIFICAR LISTA ABAIXO)
            05     BVVEWE00-CODRET            PIC 9(002).
      *            MENSAGEM DE RETORNO
            05     BVVEWE00-MSG               PIC X(080).
      *            AREA DA MENSAGEM A SER ENVIADA
            05     BVVEWE00-MENSAGEM          PIC X(21869).
