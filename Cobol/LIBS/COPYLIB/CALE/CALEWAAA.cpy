      *================================================================*        
      * COPY .....: CALEWAAA  - ( 213 BYTES )                          *        
      *                                                                *        
      * DESCRICAO.: AREA DE COMUNICACAO DOS SISTEMAS DO BRADESCO COM O *        
      *       COMPONENTE ESTRUTURAL CALENDARIO  DA NOVA ARQUITETURA DE *        
      *       SISTEMAS PARA TRATAMENTO DA FUNCIONALIDADE:              *        
      *                                                                *        
      * SF3002 .: FORNECE DATA E HORA ATUAIS      VERSAO .: 001        *        
      *                                                                *        
      * GRUPO...: DDS-GP-34            COMPONENTE CALE - CALENDARIO    *        
      *================================================================*        
      ***  DADOS DE IDENTIFICACAO DA ESTRUTURA DA SF3002             ***        
      *================================================================*        
       01  CALEWAAA.                                                            
           03  FILLER                  PIC  X(008)     VALUE 'CALEWAAA'.        
           03  FILLER                  PIC  9(005)     VALUE  213.              
           03  CALEWAAA-FUNCAO         PIC  X(006)     VALUE 'SF3002'.          
               88  CALE-SF3002              VALUE  'SF3002'.                    
           03  CALEWAAA-VERSAO         PIC  9(003)     VALUE  001.              
      *================================================================*        
      ***  DADOS PARA CONTROLE DE PROCESSAMENTO DA API               ***        
      *================================================================*        
           03  CALEWAAA-BLOCO-RETORNO.                                          
               05  CALEWAAA-COD-RET    PIC  9(002)     VALUE  ZEROS.            
               05  CALEWAAA-COD-ERRO   PIC  X(004)     VALUE  ZEROS.            
               05  CALEWAAA-COD-MSGEM  PIC  X(008)     VALUE  SPACES.           
      *================================================================*        
      ***    DADOS DE SAIDA PARA FUNCIONALIDADE SF3002               ***        
      *================================================================*        
           03  CALEWAAA-SAIDA.                                                  
               05  CALEWAAA-FORMATACAO-DATA.                                    
                   07  CALEWAAA-DT-GREGO-F-FAAMMDD                              
                                       PIC  9(007)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-GREGO-AAAAMMDD                               
                                       PIC  9(008)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-GREGO-DDMMAAAA                               
                                       PIC  9(008)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-GREGO-MMDDAAAA                               
                                       PIC  9(008)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-GREGO-B-DDMMAAAA                             
                                       PIC  X(010)     VALUE  SPACES.           
                   07  CALEWAAA-DT-GREGO-P-DDMMAAAA                             
                                       PIC  X(010)     VALUE  SPACES.           
                   07  CALEWAAA-DT-JULIANA-F-FAADDD                             
                                       PIC  9(006)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-JULIANA-AAAADDD                              
                                       PIC  9(007)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-MMAA-F-FAAMM                                 
                                       PIC  9(005)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-MMAA-AAAAMM                                  
                                       PIC  9(006)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-MMAA-MMAAAA                                  
                                       PIC  9(006)     VALUE  ZEROS.            
                   07  CALEWAAA-DT-MMAA-B-MMAAAA                                
                                       PIC  X(007)     VALUE  SPACES.           
                   07  CALEWAAA-DT-MMAA-SIGBAAAA                                
                                       PIC  X(008)     VALUE  SPACES.           
               05  CALEWAAA-INFORMACOES-HORARIO.                                
                   07  CALEWAAA-HH-HHMM                                         
                                       PIC 9(004)      VALUE  ZEROS.            
                   07  CALEWAAA-HH-2PONTOS-HHMM                                 
                                       PIC X(005)      VALUE  SPACES.           
                   07  CALEWAAA-HH-HHMMSS                                       
                                       PIC 9(006)      VALUE  ZEROS.            
                   07  CALEWAAA-HH-2PONTOS-HHMMSS                               
                                       PIC X(008)      VALUE  SPACES.           
                   07  CALEWAAA-HH-HHMMSSMMMMMM                                 
                                       PIC 9(012)      VALUE  ZEROS.            
                   07  CALEWAAA-HH-TIMESTAMP                                    
                                       PIC X(020)      VALUE  SPACES.           
                   07  CALEWAAA-HH-TIMESTAMP-DB2                                
                                       PIC X(026)      VALUE  SPACES.           
      *                                                                         
      *================================================================*        
