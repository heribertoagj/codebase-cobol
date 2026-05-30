      ******************************************************************
      * DESCRICAO.: AREA DE COMUNICACAO COM FUNCIONAL                  *
      * DATA......: 29/05/2014                                         *
      * AUTOR.....: JEFERSON PAULO DALPONTE     - CAPGEMINI - ALPHA    *
      * EMPRESA...: CAPGEMINI                                          *
      ******************************************************************
      * DATA        AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
      * XX/XX/XXXX  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      ******************************************************************
       05 CRURWI42-HEADER.
         10 CRURWI42-COD-LAYOUT            PIC  X(008) VALUE 'CRURWI42'.
         10 CRURWI42-TAM-LAYOUT            PIC  9(005) VALUE 50.
       05 CRURWI42-REGISTRO.
         10 CRURWI42-BLOCO-RETORNO.
           15 CRURWI42-COD-RETORNO             PIC  9(002).
           15 CRURWI42-COD-ERRO                PIC  X(004).
           15 CRURWI42-COD-MENSAGEM            PIC  X(008).
         10 CRURWI42-BLOCO-ENTRADA.
           15 CRURWI42-E-CEMPTO-RURAL-BACEN    PIC  9(009).
         10 CRURWI42-BLOCO-SAIDA.
           15 CRURWI42-S-CODIGO-BACEN.
             20 CRURWI42-S-CATVDD-RURAL-BACEN  PIC  9(001).
             20 CRURWI42-S-CFNALD-RURAL-BACEN  PIC  9(001).
             20 CRURWI42-S-CMODLD-SIST-BACEN   PIC  9(002).
             20 CRURWI42-S-CPRODT-RURAL-BACEN  PIC  9(004).
             20 CRURWI42-S-CVARDE-SIST-BACEN   PIC  9(003).
             20 CRURWI42-S-CCSCIO-BACEN        PIC  9(001).
             20 CRURWI42-S-CCESTA-SIST-BACEN   PIC  9(001).
             20 CRURWI42-S-CZONA-RURAL-BACEN   PIC  9(001).
