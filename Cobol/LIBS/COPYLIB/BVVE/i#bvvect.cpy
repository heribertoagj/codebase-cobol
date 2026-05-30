      ***===========================================================***
      *** NOME INC                                      LRECL=0600  ***
      *** I#BVVECT                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: AREA DE COMUNICACAO PADRAO PARA DESENVOLVIMENTO***
      ***            EM CAMADAS.                                    ***
      ***                                                           ***
      ***-----------------------------------------------------------***
      ***              DESCRICAO E VALORES POSSIVEIS                ***
      ***-----------------------------------------------------------***
      ***  BVVECT-ORIGEM                01 = IMS ON-LINE            ***
      ***                               02 = SBAT                   ***
      ***                               .....                       ***
      ***                               INDICA A ORIGEM DA CHAMADA  ***
      ***-----------------------------------------------------------***
      ***  BVVECT-VERSAO                00 = VERSAO 00              ***
      ***                               INDICA A VERSAO DO MODULO   ***
      ***-----------------------------------------------------------***
      ***                               INDICA QUAL BLOCO DE        ***
      ***                               FUNCOES SERA EXECUTADO.     ***
      ***                                                           ***
      ***-----------------------------------------------------------***
      ***  BVVECT-FUNCAO                X - INDISPONIBILIDADE BASES ***
      ***                               F - CONSISTENCIA FISICA     ***
      ***                               L - CONSISTENCIA LOGICA     ***
      ***  INDICA A FUNCAO QUE FOI      S - ACESSO CONSULTA SIMPLES ***
      ***  REALIZADA DENTRO DE UMA      C - ACESSO CONSULTA CURSOR  ***
      ***  SEQUENCIA.                   I - ACESSO INCLUSAO         ***
      ***                               D - ACESSO EXCLUSAO         ***
      ***                               U - ACESSO ATUALIZACAO      ***
      ***-----------------------------------------------------------***
      ***  BVVECT-COD-RETORNO          00 - EXECUCAO OK             ***
      ***                                   PARA TODAS AS FUNCOES   ***
      ***                              01 - EXECUCAO NAO OK         ***
      ***                                 . PARA AS FUNCOES DE CON  ***
      ***                                   SISTENCIA (F, L) INDI-  ***
      ***                                   CA  INCONSISTENCIA  EM  ***
      ***                                   ALGUMA INFORMACAO.      ***
      ***                                   SSO INDICA QUE O REGIS  ***
      ***                                   TRO NAO FOI ENCONTRADO  ***
      ***                              02 - REGISTRO DUPLICADO      ***
      ***                                  .PARA AS FUNCOES DE ACE  ***
      ***                                   SSO DE INCLUSAO E ATUA  ***
      ***                                   LIZACAO (I,U)   INDICA  ***
      ***                                   QUE A CHAVE JA EXISTE.  ***
      ***                                  .PARA AS FUNCOES DE ACE  ***
      ***                                   SSO CONSULTA SIMPLES    ***
      ***                                   (S) INDICA QUE MAIS DE  ***
      ***                                   UMA LINHA FOI ENCONTRA  ***
      ***                                   DA.                     ***
      ***                              03 - PARAMETROS INVALIDOS    ***
      ***                                  .PARA TODAS AS FUNCOES   ***
      ***                                   QUANDO ALGUM PARAMETRO  ***
      ***                                   DA AREA DE COMUNICACAO  ***
      ***                                   PADRAO FOR OMITIDO OU   ***
      ***                                   ESTIVER FORA DOS VALO   ***
      ***                                   RES ESPERADOS.          ***
      ***                                  .PARA TODAS AS FUNCOES   ***
      ***                                   OBRIGATORIO DA AREA     ***
      ***                                   DE DADOS FOR OMITIDO.   ***
      ***                              04 - ON-LINE INDISPONIVEL.   ***
      ***                              88 - ERRO GRAVE.             ***
      ***                                  .PARA TODAS AS FUNCOES   ***
      ***                                   QUANDO ALGUM ERRO NAO   ***
      ***                                   PREVISTO OCORRER EM     ***
      ***                                   ACESSO A DADOS, USO     ***
      ***                                   DE POOLS OU MODULOS.    ***
      ***-----------------------------------------------------------***
      ***  BVVECT-MENSAGEM                 .RESERVADO PARA TRANSITO ***
      ***                                   DE MENSAGENS.           ***
      ***-----------------------------------------------------------***
      ***  BVVECT-RESERVA                  .RESERVADO PARA DEMAN    ***
      ***                                   DAS FUTURAS.            ***
      ***-----------------------------------------------------------***
      ***  BVVECT-AREA-POOL7100            .RESERVADO PARA TRANS    ***
      ***                                   FERENCIA DAS INFORMA    ***
      ***                                   COES DE ERRO COLHIDAS   ***
      ***                                   NO PROGRAMA.            ***
      ***                                   FERENCIA DAS INFORMA    ***
      ***                                   COES DE ERRO DA SQLCA   ***
      ***                                   DO DB2.                 ***
      ***-----------------------------------------------------------***
      ***===========================================================***

       01  BVVECT-INC.
         03  BVVECT-ORIGEM             PIC 9(003).
         03  BVVECT-VERSAO             PIC 9(003).
         03  BVVECT-SEQUENCIA          PIC 9(002).
         03  BVVECT-FUNCAO             PIC X(001).
         03  BVVECT-COD-RETORNO        PIC 9(002).
         03  BVVECT-MENSAGEM           PIC X(079).
         03  BVVECT-RESERVA            PIC X(110).
         03  BVVECT-AREA-POOL7100      PIC X(200).
         03  BVVECT-AREA-DB2           PIC X(200).


